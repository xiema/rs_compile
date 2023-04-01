use anyhow::{Result};
use std::collections::{VecDeque};
use std::fmt::Display;

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
    pub root_id: NodeId,
}

impl Tree<'_> {
    pub fn new(grammar: &Grammar) -> Tree {
        Tree {
            nodes: Vec::new(),
            grammar: grammar,
            root_id: 0,
        }
    }

    pub fn get_child(&self, name: &str, base_node_id: NodeId) -> Option<&Node> {
        let mut q = VecDeque::new();
        q.push_front(base_node_id);

        while !q.is_empty() {
            let node_id = q.pop_front().unwrap();
            if self.grammar.elems[self.nodes[node_id].elem_id].name == name {
                return Some(&self.nodes[node_id]);
            }
            else {
                for child_id in self.nodes[node_id].children.iter().rev() {
                    q.push_front(*child_id);
                }
            }
        }

        None
    }

    pub fn iter_descendants(&self, name: &str, base_node_id: NodeId) -> Vec<NodeId> {
        let mut q = VecDeque::new();
        let mut ret = Vec::new();
        q.push_front(base_node_id);

        while !q.is_empty() {
            let node_id = q.pop_front().unwrap();
            if self.grammar.elems[self.nodes[node_id].elem_id].name == name {
                ret.push(node_id);
            }
            else {
                for child_id in self.nodes[node_id].children.iter().rev() {
                    q.push_front(*child_id);
                }
            }
        }

        ret
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, node_id: NodeId, level: usize) -> std::fmt::Result {
        let indent = String::from("  ").repeat(level);
        let node = &self.nodes[node_id];
        let elem = &self.grammar.elems[node.elem_id];
        match elem.elem_type {
            ElementType::Terminal(_) => {
                let text = &node.token.as_ref().unwrap().text;
                f.write_fmt(format_args!("{}{} >>> '{}'\n", indent, elem.name, text.replace("\n", "\\n")))?;
            },
            ElementType::NonTerminal => {
                if elem.generated {
                    for child in &self.nodes[node_id].children {
                        self.display(f, *child, level)?;
                    }
                }
                else {
                    f.write_fmt(format_args!("{}{} : [PROD {}]\n", indent, elem.name, self.nodes[node_id].prod_id.unwrap()))?;
                    for child in &self.nodes[node_id].children {
                        self.display(f, *child, level + 1)?;
                    }
                }
            },
        }
        Ok(())
    }
}

impl Display for Tree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, self.root_id, 0)
    }
}

pub trait Parser {
    fn parse(&self, tokens: &Vec<Token>) -> Result<Tree>;
    fn get_required_lookahead(&self) -> usize;
}