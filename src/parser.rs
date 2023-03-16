use std::collections::{VecDeque};

use crate::grammar::{GvarId, ProductionId, Grammar, GvarType, ParseAction};
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
        let mut stk: VecDeque<NodeId> = VecDeque::new();
        let mut pos = 0;
        nodes.push(self.new_node(0, root, None));
        stk.push_front(0);

        while !stk.is_empty() {
            let cur_node_id = stk.pop_front().unwrap();
            match grammar.gvars[nodes[cur_node_id].gvar_id].gvar_type {
                GvarType::Terminal => {
                    nodes[cur_node_id].token = Some(tokens[pos].clone());
                    pos += 1;
                },
                GvarType::NonTerminal => {
                    let prod_id = grammar.find_next(nodes[cur_node_id].gvar_id, &tokens[pos..])
                        .unwrap_or_else(|err| panic!("Parser error: {}, {}", err, tokens[pos].text));
            
                    // store production produced by this nonterm
                    nodes[cur_node_id].prod_id = Some(prod_id);
                    let prod = &grammar.gvars[nodes[cur_node_id].gvar_id].productions[prod_id];

                    for child_gvar_id in prod {
                        let new_node_id = nodes.len();
                        // create new node
                        nodes.push(self.new_node(new_node_id, *child_gvar_id, Some(cur_node_id)));
                        // associate new node as child of parent node
                        nodes[cur_node_id].children.push(new_node_id);
                    }
                    // push new nodes onto stack
                    for child_id in nodes[cur_node_id].children.iter().rev() {
                        stk.push_front(*child_id);
                    }
                }
            }
        }

        return Ok(nodes);
    }
}

struct ParserLR;

impl ParserLR {
    pub fn new() -> Self {
        Self {}
    }

    /// Creates a new node, optionally associating it with a parent.
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

impl Parser for ParserLR {
    fn parse(&mut self, grammar: &Grammar, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>, &str> {
        let mut nodes: Vec<Node> = Vec::new();

        let mut states: Vec<usize> = Vec::new();
        let mut node_stack: Vec<NodeId> = Vec::new();
        states.push(0);
        let mut pos = 0;

        nodes.push(self.new_node(0, grammar.token_gvar_map[&tokens[pos].token_type], None));

        loop {
            let next_node_id = nodes.len() - 1;
            if nodes[next_node_id].gvar_id == 0 { break; }

            let state_id = states.last().unwrap();
            let (i, map) = &grammar.parse_table_lr[*state_id][0];

            match map[&nodes[next_node_id].gvar_id] {
                ParseAction::Shift(next_state) => {
                    node_stack.push(next_node_id);
                    states.push(next_state);
                    pos += 1;
                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, grammar.token_gvar_map[&tokens[pos].token_type], None));
                },
                ParseAction::Reduce(gvar_id, prod_id) => {
                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, gvar_id, None));

                    let pop_count = grammar.gvars[gvar_id].productions[prod_id].len();
                    for _ in 0..pop_count {
                        states.pop();
                        let node_id = node_stack.pop().unwrap();
                        nodes[node_id].parent = Some(next_node_id);
                        nodes[next_node_id].children.push(node_id);
                    }
                    nodes[next_node_id].children.reverse();
                },
                ParseAction::ShiftReduce(gvar_id, prod_id) => {
                    let child_node_id = next_node_id;

                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, gvar_id, None));

                    nodes[child_node_id].parent = Some(next_node_id);
                    nodes[next_node_id].children.push(child_node_id);

                    let pop_count = grammar.gvars[gvar_id].productions[prod_id].len() - 1;
                    for _ in 0..pop_count {
                        states.pop();
                        let node_id = node_stack.pop().unwrap();
                        nodes[node_id].parent = Some(next_node_id);
                        nodes[next_node_id].children.push(node_id);
                    }
                    nodes[next_node_id].children.reverse();
                },
            }
        }

        Ok(nodes)
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
    for child in &nodes[node_id].children {
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
    fn parserll_test() {
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

    #[allow(unused_variables)]
    #[test]
    fn parserlr_test() {
        let mut tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("[[:digit:]]+"),
            TokenPattern::Single("[-+*/]"),
            TokenPattern::Single("[;]"),
        ],
            TokenPattern::Single("[[:space:]]")
        );

        let mut gram_gen = GrammarGenerator::new();
        
        gram_gen.new_nonterm("Program");
        gram_gen.new_nonterm("Expression_List");
        gram_gen.new_nonterm("Expression");
        gram_gen.new_term("Term", 0 as TokenTypeId);
        gram_gen.new_term("Operator", 1 as TokenTypeId);
        gram_gen.new_term("EndExpression", 2 as TokenTypeId);
        gram_gen.new_term("EOF", -1 as TokenTypeId);

        gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        gram_gen.make_prod("Expression_List", vec!["Expression_List", "Expression", "EndExpression"]);
        gram_gen.make_prod("Expression_List", vec!["Expression", "EndExpression"]);
        gram_gen.make_prod("Expression", vec!["Expression", "Operator", "Term"]);
        gram_gen.make_prod("Expression", vec!["Term"]);

        let gram = gram_gen.generate();

        // println!("{}", gram);

        let code = "\n1 + 1;\n\n2 + 2;\n\n3 + 1 + 2 +2;";
        let tokens = tokenizer.tokenize(code).unwrap();
        
        let mut parser = ParserLR::new();
        let nodes = parser.parse(&gram, &tokens, 0).unwrap();

        // display_ast(nodes.len()-1, &nodes, &gram, 0);
    }

    #[allow(unused_variables)]
    #[test]
    #[should_panic]
    fn lr_to_parserll() {
        let mut tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("[[:digit:]]+"),
            TokenPattern::Single("[-+*/]")
        ],
            TokenPattern::Single("[[:space:]]")
        );

        let mut gram_gen = GrammarGenerator::new();
        
        gram_gen.new_nonterm("Program");
        gram_gen.new_nonterm("Expression_List");
        gram_gen.new_nonterm("Expression");
        gram_gen.new_nonterm("Expression_Tail");
        gram_gen.new_term("Term", 0 as TokenTypeId);
        gram_gen.new_term("Operator", 1 as TokenTypeId);
        gram_gen.new_term("EOF", -1 as TokenTypeId);

        gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        gram_gen.make_prod("Expression_List", vec!["Expression_List", "Expression"]);
        gram_gen.make_prod("Expression_List", vec!["Expression"]);
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
