use super::def::*;
use crate::ir;

pub trait CFGOptimizer<Ti>
where
    Ti: Ord + Clone,
{
    fn run(&mut self, cfg: &mut CFG<Ti, ir::BasicBlock, Edge>);
}

pub struct RemoveEmptyNodes {}

impl RemoveEmptyNodes {
    fn find_removable_empty_nodes<Ti: Ord + Clone>(
        cfg: &CFG<Ti, ir::BasicBlock, Edge>,
    ) -> Option<(Ti, (Ti, Ti))> {
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
            if *n == cfg.entry || *n == cfg.exit {
                continue;
            }

            let in_nodes = cfg.in_nodes(n);
            if in_nodes.len() == 1 {
                let i = in_nodes[0];
                let ed = cfg.get_edge(i, n);
                if matches!(*ed.unwrap().borrow(), Edge::Continue) {
                    return Some((n.clone(), (i.clone(), n.clone())));
                }
            }

            let out_nodes = cfg.out_nodes(n);
            if out_nodes.len() == 1 {
                let o = out_nodes[0];
                let ed = cfg.get_edge(n, o);
                if matches!(*ed.unwrap().borrow(), Edge::Continue) {
                    return Some((n.clone(), (n.clone(), o.clone())));
                }
            }
        }
        None
    }
}

impl<Ti> CFGOptimizer<Ti> for RemoveEmptyNodes
where
    Ti: Ord + Clone,
{
    fn run(&mut self, cfg: &mut CFG<Ti, ir::BasicBlock, Edge>) {
        while let Some((n, pair)) = Self::find_removable_empty_nodes(cfg) {
            if n == pair.0 {
                let in_nodes = cfg
                    .in_nodes(&n)
                    .into_iter()
                    .map(|n| n.clone())
                    .collect::<Vec<_>>();
                for i in in_nodes {
                    let ed = cfg.remove_edge(&i, &n).unwrap();
                    cfg.insert_edge(i, pair.1.clone(), ed);
                }
            } else {
                // n == pair.1
                let out_nodes = cfg
                    .out_nodes(&n)
                    .into_iter()
                    .map(|n| n.clone())
                    .collect::<Vec<_>>();
                for o in out_nodes {
                    let ed = cfg.remove_edge(&n, &o).unwrap();
                    cfg.insert_edge(pair.0.clone(), o, ed);
                }
            }
            cfg.remove_node(&n);
        }
    }
}
