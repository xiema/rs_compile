use std::collections::VecDeque;

use crate::grammar::{GvarId, ProductionId, Grammar, GvarType};
use crate::tokenizer::{Token};

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
    fn parse(&mut self, grammar: &Grammar, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>, &str>;
}

pub struct ParserLL {}

impl ParserLL {
    pub fn new() -> Self {
        Self {}
    }

    /// Creates a new node, optionally associating it with a parent.
    /// Returns the new node's NodeId
    pub fn new_node(&mut self, new_node_id: NodeId, gvar_id: GvarId, parent: Option<NodeId>) -> Node {
        Node {
            id: new_node_id,
            gvar_id: gvar_id,
            prod_id: None,
            token: None,
            parent: parent,
            children: Vec::new(),
        }
    }
}

impl Parser for ParserLL {
    fn parse(&mut self, grammar: &Grammar, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>, &str> {
        // Grammar must not be LR
        if grammar.is_lr() {
            panic!("ParserLL can't parse LR grammar");
        }

        let mut nodes: Vec<Node> = Vec::new();
        let mut q: VecDeque<NodeId> = VecDeque::new();
        let mut pos = 0;
        nodes.push(self.new_node(0, root, None));
        q.push_front(0);

        while !q.is_empty() {
            let cur_node_id = q.pop_front().unwrap();
            match grammar.gvars[nodes[cur_node_id].gvar_id].gvar_type {
                GvarType::Terminal => {
                    nodes[cur_node_id].token = Some(tokens[pos].clone());
                    pos += 1;
                },
                GvarType::NonTerminal => {
                    let prod_id = grammar.find_next(nodes[cur_node_id].gvar_id, &tokens[pos..])
                        .unwrap_or_else(|err| panic!("Parser error: {}, {}", err, tokens[pos].text));
            
                    nodes[cur_node_id].prod_id = Some(prod_id);
                    let prod = &grammar.gvars[nodes[cur_node_id].gvar_id].productions[prod_id];

                    prod.iter().map(|node_def_id| {
                        let new_node_id = nodes.len();
                        nodes.push(self.new_node(new_node_id, *node_def_id, Some(cur_node_id)));
                        nodes[cur_node_id].children.push(new_node_id);
                        new_node_id
                    })
                    .rev().for_each(|new_node_id| q.push_front(new_node_id));
                }
            }
        }

        return Ok(nodes);
    }
}

#[allow(dead_code)]
pub fn display_ast(node_id: NodeId, nodes: &Vec<Node>, gram: &Grammar, level: usize) {
    let indent = String::from("  ").repeat(level);
    print!("{}{}", indent, gram.gvars[nodes[node_id].gvar_id].name);
    match &nodes[node_id].token {
        Some(t) => println!(" >>> '{}'", t.text),
        None => println!()
    }
    for child in nodes[node_id].children.iter().rev() {
        display_ast(*child, nodes, gram, level + 1);
    }
}


#[cfg(test)]
mod tests {
    use crate::grammar::*;
    use crate::tokenizer::*;

    use super::*;

    #[allow(unused_variables)]
    #[test]
    fn parser_test() {
        let mut tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("[[:digit:]]+"),
            TokenPattern::Single("[-+*/]")
        ],
            TokenPattern::Single("[[:space:]]")
        );

        let mut gram_gen = GrammarGenerator::new();
        
        gram_gen.new_nonterm("Program");
        gram_gen.new_nonterm("Expression_List");
        gram_gen.new_nonterm("Expression_List_Tail");
        gram_gen.new_nonterm("Expression");
        gram_gen.new_nonterm("Expression_Tail");
        gram_gen.new_term("Term", 0 as TokenTypeId);
        gram_gen.new_term("Operator", 1 as TokenTypeId);
        gram_gen.new_term("EOF", -1 as TokenTypeId);

        gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        gram_gen.make_prod("Expression_List", vec!["Expression", "Expression_List_Tail"]);
        gram_gen.make_prod("Expression_List_Tail", vec!["Expression", "Expression_List_Tail"]);
        gram_gen.make_eps("Expression_List_Tail");
        gram_gen.make_prod("Expression", vec!["Term", "Expression_Tail"]);
        gram_gen.make_prod("Expression_Tail", vec!["Operator", "Expression"]);
        gram_gen.make_eps("Expression_Tail");

        let gram = gram_gen.generate();

        // println!("{}", gram);

        let code = "\n1 + 1\n\n2 + 2\n\n3 + 1 + 2 +2";
        let tokens = tokenizer.tokenize(code).unwrap();
        
        let mut parser = ParserLL::new();
        let nodes = parser.parse(&gram, &tokens, 0).unwrap();

        // display_ast(0, &nodes, &gram, 0);
    }
}
