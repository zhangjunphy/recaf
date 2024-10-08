use super::def;
use crate::graph::Graph as G;
use graphviz_rust::attributes::*;
use graphviz_rust::dot_generator::*;
use graphviz_rust::dot_structures::*;
use graphviz_rust::printer::{DotPrinter, PrinterContext};
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

pub struct CFGDraw<'c, Ti, Tn, Te>
where
    Ti: Display + Ord + Clone,
    Tn: Display,
    Te: Display,
{
    cfg: &'c def::CFG<Ti, Tn, Te>,
}

impl<'c, Ti, Tn, Te> CFGDraw<'c, Ti, Tn, Te>
where
    Ti: Display + Ord + Clone,
    Tn: Display,
    Te: Display,
{
    pub fn new(cfg: &'c def::CFG<Ti, Tn, Te>) -> Self {
        Self { cfg }
    }

    pub fn draw(&self) -> String {
        let mut g = graph!(
            di id!("");
            node!(
                "node";
                NodeAttributes::shape(shape::box_)
            )
        );
        g.add_stmt(Stmt::Subgraph(self.draw_cfg()));
        g.print(&mut PrinterContext::default())
    }

    fn draw_cfg(&self) -> Subgraph {
        let mut stmts = Vec::new();
        for (ti, node) in self.cfg.nodes() {
            stmts.push(self.draw_node(ti, node));
        }
        for ((src, dst), edge) in self.cfg.edges() {
            stmts.push(self.draw_edge(src, dst, &edge.borrow()));
        }
        Subgraph {
            id: Id::Plain(self.cfg.name.clone()),
            stmts,
        }
    }

    fn draw_edge(&self, src: &Ti, dst: &Ti, e: &Te) -> Stmt {
        let mut edge = if src <= dst {
            edge!(node_id!(src.to_string()) => node_id!(dst.to_string()))
        } else {
            edge!(
                node_id!(dst.to_string()) => node_id!(src.to_string());
                EdgeAttributes::dir(dir::back)
            )
        };
        edge.attributes
            .push(EdgeAttributes::label(format!("\"{}\"", e)));
        Stmt::Edge(edge)
    }

    fn draw_node(&self, idx: &Ti, n: &Rc<RefCell<Tn>>) -> Stmt {
        let rank = if *idx == self.cfg.entry {
            rank::source
        } else {
            rank::same
        };

        use crate::parser::util::escape_string_literal;
        let label = format!(
            "\"{}\"",
            escape_string_literal(n.borrow().to_string().as_str())
        );
        Stmt::Node(node!(idx.to_string();
              NodeAttributes::label(label),
              SubgraphAttributes::rank(rank)
        ))
    }
}
