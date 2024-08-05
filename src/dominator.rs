//! Build a dominator tree based on the simple IBFS algorithm in
//! https://jgaa.info/accepted/2006/GeorgiadisTarjanWerneck2006.10.1.pdf
//! This should be sufficient for our current needs. Later we could
//! considier implementing the more effiencet SNCA algorithm.

use crate::cfg;
use crate::ir;
use std::collections::HashMap;
use std::collections::HashSet;

struct Node {
    parent: ir::Label,
    children: HashSet<ir::Label>,
}

struct DominatorTree {
    nodes: HashMap<ir::Label, Node>,
}

impl DominatorTree {
    pub fn new(cfg: &cfg::def::CFG<ir::Label, ir::BasicBlock, ir::Branch>) -> Self {
        let res = DominatorTree {
            nodes: HashMap::new(),
        };

        res
    }
}
