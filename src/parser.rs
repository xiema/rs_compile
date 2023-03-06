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
    fn parse(&mut self, grammar: &Grammar, tokens: &Vec<Token>) -> Result<(), &str>;
}

pub struct ParserLL {
    pos: usize,
    nodes: Vec<Node>,
    cur_node: NodeId,
}

impl ParserLL {
    pub fn new() -> Self {
        Self {
            pos: 0,
            nodes: Vec::new(),
            cur_node: 0,
        }
    }

    /// Creates a new node, optionally associating it with a parent.
    /// Returns the new node's NodeId
    pub fn new_node(&mut self, gvar_id: GvarId, parent: Option<NodeId>) -> NodeId {
        let new_node_id = self.nodes.len();
        self.nodes.push(Node {
            id: new_node_id,
            gvar_id: gvar_id,
            prod_id: None,
            token: None,
            parent: parent,
            children: Vec::new(),
        });

        // add child to parent.children
        match parent {
            None => (),
            Some(p) => {
                self.nodes[p].children.push(new_node_id);
            }
        }

        new_node_id
    }
}

impl Parser for ParserLL {
    fn parse(&mut self, grammar: &Grammar, tokens: &Vec<Token>) -> Result<(), &str> {
        // Grammar must not be LR
        if grammar.is_lr() {
            panic!("ParserLL can't parse LR grammar");
        }

        let cur_node = self.cur_node;
        match grammar.gvars[self.nodes[cur_node].gvar_id].gvar_type {
            GvarType::Terminal => {
                self.nodes[cur_node].token = Some(tokens[self.pos].clone());
                self.pos += 1;
                // println!("Parsing {}", grammar.nodes[self.nodes[cur_node].node_def].name);
                return Ok(());
            }
            ,
            GvarType::NonTerminal => {
                let res_prod = grammar.find_next(self.nodes[cur_node].gvar_id, &tokens[self.pos..])
                    .unwrap_or_else(|err| panic!("Parser error: {}, {}", err, tokens[self.pos].text));
        
                match res_prod {
                    Some(prod_id) => {
                        // expand lhs to rhs
                        self.nodes[cur_node].prod_id = Some(prod_id);
                        let prod = &grammar.gvars[self.nodes[cur_node].gvar_id].productions[prod_id];
                        for node_def_id in prod {
                            self.cur_node = self.new_node(*node_def_id, Some(cur_node));
                            match self.parse(grammar, &tokens) {
                                Ok(_) => (),
                                Err(e) => panic!("{}", e)
                            }
                        }
                    }
                    ,
                    None => {
                        // epsilon production
                    }
                }

                return Ok(());
            }
        }
    }
}

#[allow(dead_code)]
pub fn display_ast(node_id: NodeId, parser: &ParserLL, gram: &Grammar, level: usize) {
    let indent = String::from("  ").repeat(level);
    println!("{}{}", indent, gram.gvars[parser.nodes[node_id].gvar_id].name);
    for child in &parser.nodes[node_id].children {
        display_ast(*child, parser, gram, level + 1);
    }
}


#[cfg(test)]
mod tests {
    use crate::grammar::*;
    use crate::tokenizer::*;

    use super::*;

    #[test]
    fn parser_test() {
        let mut tokenizer = Tokenizer::new(
            vec![";", "[[:digit:]]+", "[-+*/]"],
            "[[:space:]]");

        let mut gram_gen = GrammarGenerator::new();
        
        gram_gen.new_nonterm("Program");
        gram_gen.new_nonterm("Statement");
        let stat_tail = gram_gen.new_nonterm("Statement_Tail");
        gram_gen.new_nonterm("Expression");
        let expr_tail = gram_gen.new_nonterm("Expression_Tail");
        gram_gen.new_term("Term", 1 as TokenTypeId);
        gram_gen.new_term("Operator", 2 as TokenTypeId);
        gram_gen.new_term("End Statement", 0 as TokenTypeId);

        gram_gen.make_prod("Program", vec!["Statement", "Statement_Tail"]);
        gram_gen.make_prod("Statement_Tail", vec!["Statement", "Statement_Tail"]);
        gram_gen.make_eps(stat_tail);
        gram_gen.make_prod("Statement", vec!["Expression", "End Statement"]);
        gram_gen.make_prod("Expression", vec!["Term", "Expression_Tail"]);
        gram_gen.make_prod("Expression_Tail", vec!["Operator", "Expression"]);
        gram_gen.make_eps(expr_tail);

        let gram = gram_gen.generate();

        println!("{}", gram);

        let code = "1 + 1; 2 + 2 ;";
        let tokens = tokenizer.tokenize(code);
        
        let mut parser = ParserLL::new();
        parser.new_node(0, None);
        match parser.parse(&gram, &tokens) {
            Ok(_) => (),
            Err(_) => (),
        };
    }
}
