pub mod dominator;
use crate::error::Error;

use std::collections::BTreeMap;

pub trait Graph<Ti, Tn, Te>
where
    Ti: Ord + PartialEq,
{
    fn out_neighbors(&self, n: &Ti) -> Vec<&Ti>;
    fn in_neighbors(&self, n: &Ti) -> Vec<&Ti>;
    fn get_node(&self, n: &Ti) -> Option<&Tn>;
    fn get_edge(&self, s: &Ti, data: &Ti) -> Option<&Te>;
    fn insert_node(&mut self, n: Ti, data: Tn) -> Option<Tn>;
    fn insert_edge(&mut self, src: Ti, dst: Ti, data: Te) -> Option<Te>;
    fn contains_node(&self, n: &Ti) -> bool;
    fn contains_edge(&self, src: &Ti, dst: &Ti) -> bool;
    fn remove_node(&mut self, n: &Ti) -> Option<Tn>;
    fn remove_edge(&mut self, src: &Ti, dst: &Ti) -> Option<Te>;
    fn nodes<'a>(&'a self) -> Vec<(&'a Ti, &'a Tn)>;
    fn edges<'a>(&'a self) -> Vec<((&'a Ti, &'a Ti), &'a Te)>;
}

impl<Ti, Tn, Te> dyn Graph<Ti, Tn, Te>
where
    Ti: Ord + PartialEq,
{
    pub fn toposort(&self) -> Result<Vec<&Ti>, Error> {
        let mut res = Vec::new();
        let mut in_degree: BTreeMap<&Ti, usize> = BTreeMap::new();
        let nodes = self.nodes();
        for (n, _) in &nodes {
            in_degree.insert(n, self.in_neighbors(n).len());
        }

        while res.len() < nodes.len() {
            if let Some((ix, _)) = in_degree.iter().find(|(_, d)| **d == 0) {
                res.push(*ix);
                for o in self.out_neighbors(*ix) {
                    in_degree.entry(o).and_modify(|c| *c -= 1);
                }
            } else {
                return Err(Error::msg("Trying to toposort a cyclic graph."));
            }
        }

        Ok(res)
    }
}
