use anyhow::{Result};

use crate::grammar::{ElementId, ProductionId, Grammar, ElementType};
use crate::tokenizer::{Token};

pub mod parserll;
pub mod parserlr;

pub use parserll::ParserLL;
pub use parserlr::ParserLR;

pub type NodeId = usize;

#[allow(dead_code)]
pub struct Node {
    pub id: NodeId,
    pub elem_id: ElementId,
    pub prod_id: Option<ProductionId>,
    pub token: Option<Token>,
    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
}

pub trait Parser {
    fn parse(&self, tokens: &Vec<Token>) -> Result<Vec<Node>>;
    fn get_required_lookahead(&self) -> usize;
}

#[allow(dead_code)]
pub fn display_tree(node_id: NodeId, nodes: &Vec<Node>, gram: &Grammar, level: usize) {
    let indent = String::from("  ").repeat(level);
    match gram.elems[nodes[node_id].elem_id].elem_type {
        ElementType::Terminal(_) => {
            print!("{}{}", indent, gram.elems[nodes[node_id].elem_id].name);
        },
        ElementType::NonTerminal => {
            print!("{}{} : [PROD {}]", indent, gram.elems[nodes[node_id].elem_id].name, nodes[node_id].prod_id.unwrap());
        }
    }
    match &nodes[node_id].token {
        Some(t) => println!(" >>> '{}'", t.text),
        None => println!()
    }
    for child in &nodes[node_id].children {
        display_tree(*child, nodes, gram, level + 1);
    }
}