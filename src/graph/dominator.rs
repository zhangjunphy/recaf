//! Build a dominator tree based on the simple IBFS algorithm in
//! https://jgaa.info/accepted/2006/GeorgiadisTarjanWerneck2006.10.1.pdf
//! This should be sufficient for our current needs. Later we could
//! consider implementing the more effiencet SNCA algorithm.

use super::Graph;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

pub struct Node<'a, Ti>
where
    Ti: Ord,
{
    pub label: &'a Ti,
    pub parent: Option<&'a Ti>,
    pub children: BTreeSet<&'a Ti>,
}

impl<'a, Ti> Node<'a, Ti>
where
    Ti: Ord,
{
    pub fn new(label: &'a Ti, parent: Option<&'a Ti>, children: BTreeSet<&'a Ti>) -> Self {
        Node {
            label,
            parent,
            children,
        }
    }
}

pub struct DominatorTree<'a, Ti>
where
    Ti: Ord,
{
    root: &'a Ti,
    nodes: BTreeMap<&'a Ti, Node<'a, Ti>>,
}

impl<'a, Ti> DominatorTree<'a, Ti>
where
    Ti: Ord,
{
    pub fn new<BB, EG>(entry: &'a Ti, graph: &'a dyn Graph<Ti, BB, EG>) -> Self {
        let mut tree = DominatorTree {
            root: entry,
            nodes: BTreeMap::new(),
        };

        // Do a bfs walk of the cfg, construct a tree
        let mut queue = VecDeque::from([entry]);
        let mut parent_map = BTreeMap::new();
        while let Some(n) = queue.pop_front() {
            let mut out_nodes = BTreeSet::new();
            for o in graph.out_neighbors(n) {
                if parent_map.contains_key(o) {
                    continue;
                }
                out_nodes.insert(o);
                parent_map.insert(o, n);
                queue.push_back(o);
            }
            tree.add(n, None, out_nodes);
        }

        // Patch parent
        for (n, p) in &parent_map {
            tree.get_mut(n).unwrap().parent = Some(p);
        }

        // Iteratively update parents of all nodes
        let nodes = tree.nodes.iter().map(|(n, _)| *n).collect::<Vec<_>>();
        loop {
            let mut changed = false;
            for n in &nodes {
                let preds = graph.in_neighbors(n);
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

    pub fn root(&self) -> &'a Ti {
        &self.root
    }

    pub fn strictly_dominates(&self, n: &Ti) -> Vec<&'a Ti> {
        let mut res = Vec::new();
        for c in &self.nodes.get(n).unwrap().children {
            res.push(*c);
            let mut c_doms = self.strictly_dominates(c);
            res.append(&mut c_doms);
        }
        res
    }

    pub fn dominates(&self, n: &'a Ti) -> Vec<&'a Ti> {
        let mut res = vec![n];
        res.append(&mut self.strictly_dominates(n));
        res
    }

    fn add(&mut self, l: &'a Ti, p: Option<&'a Ti>, c: BTreeSet<&'a Ti>) {
        self.nodes.insert(l, Node::new(l, p, c));
    }

    fn get_mut(&mut self, l: &Ti) -> Option<&mut Node<'a, Ti>> {
        self.nodes.get_mut(l)
    }

    fn get_parent(&self, l: &Ti) -> Option<&'a Ti> {
        self.nodes.get(l).and_then(|n| n.parent)
    }

    fn replace_parent(&mut self, l: &'a Ti, p: &'a Ti) {
        if let Some(prev_parent) = self.get_parent(l) {
            self.nodes.get_mut(&prev_parent).unwrap().children.remove(l);
        }

        self.nodes.get_mut(p).unwrap().children.insert(l);
        self.nodes.get_mut(l).unwrap().parent = Some(p);
    }

    fn nearest_common_ancestor(&self, nodes: Vec<&Ti>) -> &'a Ti {
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

    fn path_to_root(&self, n: &Ti) -> Vec<&'a Ti> {
        let mut res = Vec::new();
        let nn = self.nodes.get_key_value(n).unwrap().0;
        let mut node = Some(*nn);
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

pub struct DominanceFrontier<'a, Ti>
where
    Ti: Ord,
{
    root: &'a Ti,
    nodes: BTreeMap<&'a Ti, Vec<&'a Ti>>,
}

impl<'a, Ti> DominanceFrontier<'a, Ti>
where
    Ti: Ord + PartialEq,
{
    pub fn new<BB, EG>(entry: &'a Ti, cfg: &'a dyn Graph<Ti, BB, EG>) -> Self {
        let dt = DominatorTree::new(entry, cfg);
        let mut nodes: BTreeMap<&'a Ti, Vec<&'a Ti>> = BTreeMap::new();
        for (n, _) in &dt.nodes {
            let n_doms = dt.dominates(n).into_iter().collect::<BTreeSet<_>>();
            let df_nodes = n_doms
                .iter()
                .flat_map(|nd| {
                    let nd_succ = cfg.out_neighbors(nd);
                    nd_succ
                        .into_iter()
                        .filter(|su| su == n || !n_doms.contains(su))
                })
                .collect();
            nodes.insert(*n, df_nodes);
        }

        DominanceFrontier { root: entry, nodes }
    }

    pub fn root(&self) -> &'a Ti {
        &self.root
    }

    pub fn get_frontier(&self, n: &Ti) -> Vec<&'a Ti> {
        self.nodes
            .get(n)
            .into_iter()
            .flat_map(|v| v.iter().map(|l| *l).collect::<Vec<_>>())
            .collect()
    }
}
