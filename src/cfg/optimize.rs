use super::def::*;
use crate::graph::Graph;
use crate::ir;

pub trait CFGOptimizer {
    fn run(&mut self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>);
}

pub struct RemoveEmptyNodes {}

impl RemoveEmptyNodes {
    fn br_only(stmts: &Vec<ir::Statement>) -> bool {
        if stmts.is_empty() {
            return true;
        }
        if stmts.len() > 1 {
            return false;
        }

        let stmt = &stmts[0];
        match stmt {
            ir::Statement::Br(ir::Branch::UnCon { bb: _ }) => true,
            _ => false,
        }
    }

    fn find_removable_empty_nodes(
        cfg: &CFG<ir::Label, ir::BasicBlock, Edge>,
    ) -> Option<(ir::Label, ir::Label)> {
        let empty_nodes = cfg
            .nodes()
            .into_iter()
            .filter(|(_, n)| {
                let nd = (*n).borrow();
                nd.args.is_empty() && Self::br_only(&nd.statements)
            })
            .map(|(i, _)| i);

        // Currently we can only remove nodes with one outgoing Edge::Continue.
        for n in empty_nodes {
            if *n == cfg.entry || *n == cfg.exit {
                continue;
            }

            let out_nodes = cfg.out_neighbors(n);
            if out_nodes.len() != 1 {
                continue;
            }
            let o = out_nodes[0];
            if cfg.in_neighbors(o).len() != 1 {
                continue;
            }
            let ed = cfg.get_edge(n, o);
            if matches!(*ed.unwrap().borrow(), Edge::Continue) {
                return Some(((*n).clone(), o.clone()));
            }
        }
        None
    }
}

impl CFGOptimizer for RemoveEmptyNodes {
    fn run(&mut self, cfg: &mut CFG<ir::Label, ir::BasicBlock, Edge>) {
        while let Some((n_empty, o)) = Self::find_removable_empty_nodes(cfg) {
            let in_nodes = cfg
                .in_neighbors(&n_empty)
                .into_iter()
                .map(|n| n.clone())
                .collect::<Vec<_>>();
            for i in in_nodes {
                {
                    let mut bb_mut = cfg.get_node(&i).unwrap().borrow_mut();
                    let br = bb_mut.statements.last_mut().unwrap();
                    match br {
                        ir::Statement::Br(ir::Branch::UnCon {
                            bb: ir::CallBB { label, args: _ },
                        }) => *label = o,
                        ir::Statement::Br(ir::Branch::Con {
                            pred: _,
                            bb_true:
                                ir::CallBB {
                                    label: l_true,
                                    args: _,
                                },
                            bb_false:
                                ir::CallBB {
                                    label: l_false,
                                    args: _,
                                },
                        }) => {
                            if *l_true == n_empty {
                                *l_true = o;
                            }
                            if *l_false == n_empty {
                                *l_false = o;
                            }
                        }
                        _ => panic!("Basic block with non-terminal last statement."),
                    }
                }
                let ed = cfg.remove_edge(&i, &n_empty).unwrap();
                cfg.insert_edge(i, o, ed);
            }
            cfg.remove_edge(&n_empty, &o).unwrap();
            cfg.remove_node(&n_empty);
        }
    }
}
