//! Build a dominator tree based on the simple IBFS algorithm in
//! https://jgaa.info/accepted/2006/GeorgiadisTarjanWerneck2006.10.1.pdf
//! This should be sufficient for our current needs. Later we could
//! considier implementing the more effiencet SNCA algorithm.

use crate::cfg::def::CFG;
use crate::ir;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub struct Node {
    pub label: ir::Label,
    pub parent: Option<ir::Label>,
    pub children: BTreeSet<ir::Label>,
}

impl Node {
    pub fn new(label: ir::Label, parent: Option<ir::Label>, children: BTreeSet<ir::Label>) -> Self {
        Node {
            label,
            parent,
            children,
        }
    }
}

pub struct DominatorTree {
    root: ir::Label,
    nodes: HashMap<ir::Label, Node>,
}

impl DominatorTree {
    pub fn new<BB, EG>(cfg: &CFG<ir::Label, BB, EG>) -> Self {
        let mut tree = DominatorTree {
            root: cfg.entry,
            nodes: HashMap::new(),
        };

        // Do a bfs walk of the cfg, construct a tree
        let mut queue = VecDeque::from([&cfg.entry]);
        let mut parent_map = HashMap::new();
        while let Some(n) = queue.pop_front() {
            let mut out_nodes = BTreeSet::new();
            for o in cfg.out_nodes(n) {
                if parent_map.contains_key(o) {
                    continue;
                }
                out_nodes.insert(*o);
                parent_map.insert(*o, *n);
                queue.push_back(o);
            }
            tree.add(*n, None, out_nodes);
        }

        // Patch parent
        for (n, p) in &parent_map {
            tree.get_mut(n).unwrap().parent = Some(*p);
        }

        // Iteratively update parents of all nodes
        let nodes = tree.nodes.iter().map(|(n, _)| *n).collect::<Vec<_>>();
        loop {
            let mut changed = false;
            for n in &nodes {
                let preds = cfg.in_nodes(n);
                if preds.is_empty() {
                    continue;
                }
                let nca = tree.nearest_common_ancestor(preds);
                if nca != tree.get_parent(n).unwrap() {
                    tree.replace_parent(n, &nca);
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }

        tree
    }

    pub fn root(&self) -> &ir::Label {
        &self.root
    }

    pub fn strictly_dominates(&self, n: &ir::Label) -> Vec<ir::Label> {
        let mut res = Vec::new();
        for c in &self.nodes.get(n).unwrap().children {
            res.push(*c);
            let mut c_doms = self.strictly_dominates(c);
            res.append(&mut c_doms);
        }
        res
    }

    pub fn dominates(&self, n: &ir::Label) -> Vec<ir::Label> {
        let mut res = vec![*n];
        res.append(&mut self.strictly_dominates(n));
        res
    }

    fn add(&mut self, l: ir::Label, p: Option<ir::Label>, c: BTreeSet<ir::Label>) {
        self.nodes.insert(l, Node::new(l, p, c));
    }

    fn get_mut(&mut self, l: &ir::Label) -> Option<&mut Node> {
        self.nodes.get_mut(l)
    }

    fn get_parent(&self, l: &ir::Label) -> Option<ir::Label> {
        self.nodes.get(l).and_then(|n| n.parent)
    }

    fn replace_parent(&mut self, l: &ir::Label, p: &ir::Label) {
        if let Some(prev_parent) = self.get_parent(l) {
            self.nodes.get_mut(&prev_parent).unwrap().children.remove(l);
        }

        self.nodes.get_mut(p).unwrap().children.insert(*l);
        self.nodes.get_mut(l).unwrap().parent = Some(*p);
    }

    fn nearest_common_ancestor(&self, nodes: Vec<&ir::Label>) -> ir::Label {
        assert!(nodes.len() > 0);
        let paths = nodes
            .into_iter()
            .map(|n| self.path_to_root(n))
            .collect::<Vec<_>>();
        let mut i = 0;
        loop {
            if paths[0].len() <= i {
                break;
            }
            if !paths.iter().all(|p| p.len() > i && p[i] == paths[0][i]) {
                break;
            }
            i += 1;
        }
        paths[0][i - 1]
    }

    fn path_to_root(&self, n: &ir::Label) -> Vec<ir::Label> {
        let mut res = Vec::new();
        let mut node = Some(*n);
        loop {
            if !node.is_some() {
                break;
            }
            res.push(node.unwrap());
            node = self.get_parent(&node.unwrap());
        }
        res.reverse();
        res
    }
}

pub struct DominanceFrontier {
    root: ir::Label,
    nodes: HashMap<ir::Label, Vec<ir::Label>>,
}

impl DominanceFrontier {
    pub fn new<BB, EG>(cfg: &CFG<ir::Label, BB, EG>) -> Self {
        let dt = DominatorTree::new(cfg);
        let mut nodes: HashMap<ir::Label, Vec<ir::Label>> = HashMap::new();
        for (n, _) in &dt.nodes {
            let n_doms = dt.dominates(n).into_iter().collect::<HashSet<_>>();
            let df_nodes = n_doms
                .iter()
                .flat_map(|nd| {
                    let nd_succ = cfg.out_nodes(nd);
                    nd_succ
                        .into_iter()
                        .filter(|su| *su == n || !n_doms.contains(su))
                        .map(|n| *n)
                })
                .collect();
            nodes.insert(*n, df_nodes);
        }

        DominanceFrontier {
            root: cfg.entry,
            nodes,
        }
    }

    pub fn root(&self) -> &ir::Label {
        &self.root
    }

    pub fn get_frontier(&self, n: &ir::Label) -> Vec<&ir::Label> {
        self.nodes
            .get(n)
            .into_iter()
            .flat_map(|v| v.iter().map(|l| l).collect::<Vec<&ir::Label>>())
            .collect()
    }
}
