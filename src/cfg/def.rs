use crate::ir;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

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

    pub fn in_nodes(&self, n: &Ti) -> Vec<&Ti> {
        if let Some(nodes) = self.redges.get(n) {
            nodes.iter().collect()
        } else {
            Vec::new()
        }
    }

    pub fn out_nodes(&self, n: &Ti) -> Vec<&Ti> {
        if let Some(edges) = self.edges.get(n) {
            edges.iter().map(|(i, _)| i).collect()
        } else {
            Vec::new()
        }
    }

    pub fn node_data(&self, n: &Ti) -> Option<&Rc<RefCell<Tn>>> {
        self.nodes.get(n)
    }

    pub fn edge_data(&self, src: &Ti, dst: &Ti) -> Option<&Rc<RefCell<Te>>> {
        let edges = self.edges.get(src)?;
        edges.get(dst)
    }

    /// Removes node n and all its related edges.
    pub fn remove_node(&mut self, n: &Ti) -> Option<Rc<RefCell<Tn>>> {
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

    pub fn contains_node(&self, n: &Ti) -> bool {
        self.nodes.contains_key(n)
    }

    pub fn contains_edge(&self, src: &Ti, dst: &Ti) -> bool {
        if let Some(edges) = self.edges.get(src) {
            edges.contains_key(dst)
        } else {
            false
        }
    }

    pub fn remove_edge(&mut self, src: &Ti, dst: &Ti) -> Option<Rc<RefCell<Te>>> {
        let redges = self.redges.get_mut(dst)?;
        redges.remove(src);
        let edges = self.edges.get_mut(src)?;
        edges.remove(dst)
    }

    pub fn insert_node(&mut self, n: Ti, d: Rc<RefCell<Tn>>) -> Option<Rc<RefCell<Tn>>> {
        self.nodes.insert(n, d)
    }

    pub fn insert_edge(&mut self, src: Ti, dst: Ti, e: Rc<RefCell<Te>>) -> Option<Rc<RefCell<Te>>> {
        let redges = self.redges.entry(dst.clone()).or_insert(BTreeSet::new());
        redges.insert(src.clone());
        let edges = self.edges.entry(src).or_insert(BTreeMap::new());
        edges.insert(dst, e)
    }

    pub fn nodes(&self) -> &BTreeMap<Ti, Rc<RefCell<Tn>>> {
        &self.nodes
    }

    pub fn edges(&self) -> &BTreeMap<Ti, BTreeMap<Ti, Rc<RefCell<Te>>>> {
        &self.edges
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
    pub globals: Vec<Rc<ir::VVar>>,
    pub cfgs: HashMap<String, CFG<ir::Label, ir::BasicBlock, Edge>>,
}
