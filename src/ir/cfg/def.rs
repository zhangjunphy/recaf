use crate::ast;
use crate::graph::Graph;
use crate::ir;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct CFG<Ti, Tn, Te>
where
    Ti: Ord + Clone,
{
    pub name: String,
    pub entry: Ti,
    pub exit: Ti,
    nodes: BTreeMap<Ti, Rc<RefCell<Tn>>>,
    edges: BTreeMap<Ti, BTreeMap<Ti, Rc<RefCell<Te>>>>,
    redges: BTreeMap<Ti, BTreeSet<Ti>>,
}

impl<Ti, Tn, Te> Graph<Ti, Rc<RefCell<Tn>>, Rc<RefCell<Te>>> for CFG<Ti, Tn, Te>
where
    Ti: Ord + Clone,
{
    fn in_neighbors(&self, n: &Ti) -> Vec<&Ti> {
        if let Some(nodes) = self.redges.get(n) {
            nodes.iter().collect()
        } else {
            Vec::new()
        }
    }

    fn out_neighbors(&self, n: &Ti) -> Vec<&Ti> {
        if let Some(edges) = self.edges.get(n) {
            edges.iter().map(|(i, _)| i).collect()
        } else {
            Vec::new()
        }
    }

    fn get_node(&self, n: &Ti) -> Option<&Rc<RefCell<Tn>>> {
        self.nodes.get(n)
    }

    fn get_edge(&self, src: &Ti, dst: &Ti) -> Option<&Rc<RefCell<Te>>> {
        let edges = self.edges.get(src)?;
        edges.get(dst)
    }

    /// Removes node n and all its related edges.
    fn remove_node(&mut self, n: &Ti) -> Option<Rc<RefCell<Tn>>> {
        let nd = self.nodes.remove(n);
        self.edges.remove(n);
        self.edges.iter_mut().for_each(|(_, m)| {
            m.remove(&n);
        });
        self.redges.remove(n);
        self.redges.iter_mut().for_each(|(_, s)| {
            s.remove(&n);
        });
        nd
    }

    fn remove_edge(&mut self, src: &Ti, dst: &Ti) -> Option<Rc<RefCell<Te>>> {
        let redges = self.redges.get_mut(dst)?;
        redges.remove(src);
        let edges = self.edges.get_mut(src)?;
        edges.remove(dst)
    }

    fn contains_node(&self, n: &Ti) -> bool {
        self.nodes.contains_key(n)
    }

    fn contains_edge(&self, src: &Ti, dst: &Ti) -> bool {
        if let Some(edges) = self.edges.get(src) {
            edges.contains_key(dst)
        } else {
            false
        }
    }

    fn insert_node(&mut self, n: Ti, data: Rc<RefCell<Tn>>) -> Option<Rc<RefCell<Tn>>> {
        self.nodes.insert(n, data)
    }

    fn insert_edge(&mut self, src: Ti, dst: Ti, data: Rc<RefCell<Te>>) -> Option<Rc<RefCell<Te>>> {
        let redges = self.redges.entry(dst.clone()).or_insert(BTreeSet::new());
        redges.insert(src.clone());
        let edges = self.edges.entry(src).or_insert(BTreeMap::new());
        edges.insert(dst, data)
    }

    fn nodes(&self) -> Vec<(&Ti, &Rc<RefCell<Tn>>)> {
        self.nodes.iter().map(|(ix, n)| (ix, n)).collect()
    }

    fn edges(&self) -> Vec<((&Ti, &Ti), &Rc<RefCell<Te>>)> {
        let mut res = Vec::new();
        self.edges.iter().for_each(|(s, map_to)| {
            map_to.iter().for_each(|(t, e)| {
                res.push(((s, t), e));
            })
        });
        res
    }
}

impl<Ti, Tn, Te> CFG<Ti, Tn, Te>
where
    Ti: Ord + Clone,
{
    pub fn new(name: String, entry: Ti, exit: Ti) -> Self {
        CFG {
            name,
            entry,
            exit,
            nodes: BTreeMap::new(),
            edges: BTreeMap::new(),
            redges: BTreeMap::new(),
        }
    }
}

pub enum Edge {
    Continue,
    JumpTrue(ir::Val),
    JumpFalse(ir::Val),
}

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Edge::Continue => f.write_str(""),
            Edge::JumpTrue(v) => f.write_str(v.to_string().as_str()),
            Edge::JumpFalse(v) => write!(f, "!{}", v),
        }
    }
}

pub struct Program {
    pub imports: Vec<String>,
    pub globals: Vec<(ir::VVar, Option<ast::Literal>)>,
    pub cfgs: HashMap<String, CFG<ir::Label, ir::BasicBlock, Edge>>,
}

impl Program {
    fn linearize_cfg(
        cfg: &CFG<ir::Label, ir::BasicBlock, Edge>,
        decl: &ast::MethodDecl,
    ) -> ir::Function {
        // NOTE: Arguments of the entry block should just be the function arguments.
        let args = cfg.get_node(&cfg.entry).unwrap().borrow().args.clone();

        // Do a toposort of cfg nodes, but try to put nodes in a
        // sequential jmp chain close to minimize jmps.
        // Also we ignore back edges introduced by loops so it could behave like
        // an acyclic graph.
        let mut body = Vec::new();
        {
            let mut in_degree: BTreeMap<&ir::Label, usize> = BTreeMap::new();
            let mut visited: BTreeSet<_> = BTreeSet::new();
            let nodes = cfg.nodes();
            for (n, _) in &nodes {
                // NOTE: Only count nodes with smaller idx to ignore back edges.
                in_degree.insert(n, cfg.in_neighbors(n).into_iter().filter(|i| i < n).count());
            }

            let mut last_node_opt = None;
            while body.len() < nodes.len() {
                // Favoring any direct offspring of the last node to reduce jmp.
                let mut next_opt = None;
                if let Some(last_node) = last_node_opt {
                    for out in cfg.out_neighbors(last_node) {
                        if !visited.contains(out) && in_degree.get(out).is_some_and(|d| *d == 0) {
                            next_opt = Some(out);
                            break;
                        }
                    }
                }
                if next_opt.is_none() {
                    if let Some((ix, _)) = in_degree
                        .iter()
                        .find(|(ix, d)| **d == 0 && !visited.contains(**ix))
                    {
                        next_opt = Some(ix);
                    }
                }

                if let Some(next) = next_opt {
                    last_node_opt = Some(next);
                    visited.insert(next);
                    body.push(cfg.get_node(next).unwrap().take());
                    for o in cfg.out_neighbors(next) {
                        // Requiring o > next filters out backward edges.
                        // As we did not count them in in_degree in the first place.
                        if o > next {
                            in_degree.entry(o).and_modify(|c| *c -= 1);
                        }
                    }
                } else {
                    panic!("Control flow graph is cyclic after dropping backward edges.")
                }
            }
        }

        ir::Function {
            name: cfg.name.clone(),
            args,
            ty: decl.ty.clone(),
            body,
        }
    }

    pub fn linearize(self, p: &ast::Program) -> ir::Module {
        let mut functions = Vec::new();
        for m in &p.methods {
            let cfg = self.cfgs.get(&m.id.str).unwrap();
            functions.push(Self::linearize_cfg(cfg, m));
        }

        ir::Module {
            imports: self.imports,
            globals: self.globals,
            functions,
        }
    }
}
