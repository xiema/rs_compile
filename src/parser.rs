use anyhow::{Result};
use std::collections::{VecDeque};

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

pub struct Tree<'a> {
    pub nodes: Vec<Node>,
    pub grammar: &'a Grammar,
}

impl Tree<'_> {
    pub fn new(grammar: &Grammar) -> Tree {
        Tree {
            nodes: Vec::new(),
            grammar: grammar,
        }
    }
}

pub trait Parser {
    fn parse(&self, tokens: &Vec<Token>) -> Result<Tree>;
    fn get_required_lookahead(&self) -> usize;
}

#[allow(dead_code)]
pub fn display_tree(node_id: NodeId, nodes: &Vec<Node>, gram: &Grammar, level: usize) {
    let indent = String::from("  ").repeat(level);
    let node = &nodes[node_id];
    let elem = &gram.elems[node.elem_id];
    match elem.elem_type {
        ElementType::Terminal(_) => {
            println!("{}{} >>> '{}'", indent, elem.name, node.token.as_ref().unwrap().text);
        },
        ElementType::NonTerminal => {
            if elem.generated {
                for child in &nodes[node_id].children {
                    display_tree(*child, nodes, gram, level);
                }
            }
            else {
                println!("{}{} : [PROD {}]", indent, elem.name, nodes[node_id].prod_id.unwrap());
                for child in &nodes[node_id].children {
                    display_tree(*child, nodes, gram, level + 1);
                }
            }
        },
    }
}