use super::def::*;
use super::partial;
use crate::ir;

pub trait CFGOptimizer<Ti>
where
    Ti: Ord,
{
    fn run(&mut self, cfg: &mut CFG<Ti, ir::BasicBlock, partial::Edge>);
}

pub struct RemoveEmptyNodes {}

impl RemoveEmptyNodes {
    fn find_removable_empty_nodes<Ti: Ord>(
        cfg: &CFG<Ti, ir::BasicBlock, partial::Edge>,
    ) -> Option<(&Ti, (&Ti, &Ti))> {
        let empty_nodes = cfg
            .nodes()
            .iter()
            .filter(|(_, n)| {
                let nd = (*n).borrow();
                nd.args.is_empty() && nd.statements.is_empty()
            })
            .map(|(i, _)| i);

        // Currently we can only remove nodes with one incoming or outgoing Edge::Continue.
        for n in empty_nodes {
            let in_nodes = cfg.in_nodes(n);
            if in_nodes.len() == 1 {
                let i = in_nodes[0];
                let ed = cfg.edge_data(i, n);
                if matches!(*ed.unwrap().borrow(), partial::Edge::Continue) {
                    return Some((n, (i, n)));
                }
            }

            let out_nodes = cfg.out_nodes(n);
            if out_nodes.len() == 1 {
                let o = out_nodes[0];
                let ed = cfg.edge_data(n, o);
                if matches!(*ed.unwrap().borrow(), partial::Edge::Continue) {
                    return Some((n, (n, o)));
                }
            }
        }
        None
    }
}

impl<Ti> CFGOptimizer<Ti> for RemoveEmptyNodes
where
    Ti: Ord,
{
    fn run(&mut self, cfg: &mut CFG<Ti, ir::BasicBlock, partial::Edge>) {
        while let Some((n, pair)) = Self::find_removable_empty_nodes(cfg) {}
    }
}
