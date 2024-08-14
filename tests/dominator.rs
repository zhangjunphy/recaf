use recaf::cfg::def::{CFG, Graph};
use recaf::dominator::*;
use recaf::ir;
use std::cell::RefCell;
use std::rc::Rc;

fn sample_cfg() -> CFG<ir::Label, (), ()> {
    let new_data = || Rc::new(RefCell::new(()));
    let mut cfg = CFG::new("test".to_string(), ir::Label::new(0), ir::Label::new(3));
    for l in 0..4 {
        cfg.insert_node(ir::Label::new(l), new_data());
    }
    for (s, d) in [(0, 1), (0, 2), (1, 3), (2, 3)] {
        cfg.insert_edge(ir::Label::new(s), ir::Label::new(d), new_data());
    }
    cfg
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dominator_tree() {
        let cfg = sample_cfg();
        let dt = DominatorTree::new(&cfg);
        {
            let res = dt
                .strictly_dominates(&ir::Label::new(0))
                .into_iter()
                .map(|l| l.id)
                .collect::<Vec<_>>();
            assert!(res == vec![1, 2, 3]);
        }
        {
            let res = dt
                .dominates(&ir::Label::new(0))
                .into_iter()
                .map(|l| l.id)
                .collect::<Vec<_>>();
            assert!(res == vec![0, 1, 2, 3]);
        }
    }

    #[test]
    fn test_dominance_frontier() {
        let cfg = sample_cfg();
        let df = DominanceFrontier::new(&cfg);
        assert!(df.get_frontier(&ir::Label::new(0)).is_empty());
        let res = df
            .get_frontier(&ir::Label::new(1))
            .into_iter()
            .map(|l| l.id)
            .collect::<Vec<_>>();
        assert!(res == vec![3]);
        assert!(
            df.get_frontier(&ir::Label::new(2))
                .into_iter()
                .map(|l| l.id)
                .collect::<Vec<_>>()
                == vec![3]
        );
    }
}
