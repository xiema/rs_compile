use anyhow::{Result};

use crate::grammar::{GvarId, ProductionId, Grammar, GvarType};
use crate::tokenizer::{Token};

pub mod parserll;
pub mod parserlr;

pub use parserll::ParserLL;
pub use parserlr::ParserLR;

type NodeId = usize;

#[allow(dead_code)]
pub struct Node {
    id: NodeId,
    gvar_id: GvarId,
    prod_id: Option<ProductionId>,
    token: Option<Token>,
    parent: Option<NodeId>,
    children: Vec<NodeId>,
}

pub trait Parser {
    fn parse(&self, tokens: &Vec<Token>) -> Result<Vec<Node>>;
    fn get_required_lookahead(&self) -> usize;
}

#[allow(dead_code)]
pub fn display_tree(node_id: NodeId, nodes: &Vec<Node>, gram: &Grammar, level: usize) {
    let indent = String::from("  ").repeat(level);
    match gram.gvars[nodes[node_id].gvar_id].gvar_type {
        GvarType::Terminal => {
            print!("{}{}", indent, gram.gvars[nodes[node_id].gvar_id].name);
        },
        GvarType::NonTerminal => {
            print!("{}{} : [PROD {}]", indent, gram.gvars[nodes[node_id].gvar_id].name, nodes[node_id].prod_id.unwrap());
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