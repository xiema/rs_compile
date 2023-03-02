use crate::grammar::{NodeDefId, ProductionId, Grammar, NodeType};

type NodeId = usize;

pub struct Node {
    id: NodeId,
    node_def: NodeDefId,
    prod_id: Option<ProductionId>,
    token: Option<String>,
    parent: Option<NodeId>,
    children: Vec<NodeId>,
}

pub trait Parser {
    fn parse(&mut self, grammar: &Grammar, tokens: &Vec<String>) -> Result<(), &str>;
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

    /// Creates a new node (allocated in Parser.nodes), optionally associating it with a parent.
    /// Returns the new node's NodeId
    pub fn new_node(&mut self, node_def: NodeDefId, parent: Option<NodeId>) -> NodeId {
        let new_node_id = self.nodes.len();
        self.nodes.push(Node {
            id: new_node_id,
            node_def: node_def,
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

    fn try_get_token<'a>(&self, tokens: &'a Vec<String>) -> Option<&'a str> {
        if self.pos < tokens.len() {
            Some(tokens[self.pos].as_str())
        }
        else {
            None
        }
    }
}

impl Parser for ParserLL {
    fn parse(&mut self, grammar: &Grammar, tokens: &Vec<String>) -> Result<(), &str> {
        // Grammar must not be LR
        if grammar.is_lr() {
            panic!("ParserLL can't parse LR grammar");
        }

        let cur_node = self.cur_node;
        let node_type = grammar.nodes[self.nodes[cur_node].node_def].node_type;
        match node_type {
            NodeType::Terminal => {
                self.nodes[cur_node].token = Some(String::from(&tokens[self.pos]));
                self.pos += 1;
                // println!("Parsing {}", grammar.nodes[self.nodes[cur_node].node_def].name);
                return Ok(());
            }
            ,
            NodeType::NonTerminal => {
                let token = self.try_get_token(tokens);
                let res_prod = grammar.find_next(self.nodes[cur_node].node_def, token)
                    .unwrap_or_else(|err| panic!("Parser error: {}", err));
        
                match res_prod {
                    Some(prod_id) => {
                        // expand lhs to rhs
                        self.nodes[cur_node].prod_id = Some(prod_id);
                        let prod = &grammar.nodes[self.nodes[cur_node].node_def].productions[prod_id];
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

pub fn display_ast(node_id: NodeId, parser: &ParserLL, gram: &Grammar, level: usize) {
    let indent = String::from("  ").repeat(level);
    println!("{}{}", indent, gram.nodes[parser.nodes[node_id].node_def].name);
    for child in &parser.nodes[node_id].children {
        display_ast(*child, parser, gram, level + 1);
    }
}


#[cfg(test)]
mod tests {
    use crate::grammar::GrammarGenerator;

    use super::*;

    #[test]
    fn parser_test() {
        let mut gram_gen = GrammarGenerator::new();
        
        gram_gen.new_nonterm("Program");
        gram_gen.new_nonterm("Statement");
        let stat_tail = gram_gen.new_nonterm("Statement_Tail");
        gram_gen.new_nonterm("Expression");
        let expr_tail = gram_gen.new_nonterm("Expression_Tail");
        gram_gen.new_term("Term", "[[:digit:]]+");
        gram_gen.new_term("Operator", "[-/+*]");
        gram_gen.new_term("End Statement", ";");

        gram_gen.make_prod("Program", vec!["Statement", "Statement_Tail"]);
        gram_gen.make_prod("Statement_Tail", vec!["Statement", "Statement_Tail"]);
        gram_gen.make_eps(stat_tail);
        gram_gen.make_prod("Statement", vec!["Expression", "End Statement"]);
        gram_gen.make_prod("Expression", vec!["Term", "Expression_Tail"]);
        gram_gen.make_prod("Expression_Tail", vec!["Operator", "Expression"]);
        gram_gen.make_eps(expr_tail);

        let gram = gram_gen.generate();

        println!("{}", gram);

        let mut parser = ParserLL::new();
        let code = vec!["1", "+", "1", ";", "2", "+", "2", ";"];
        let code = code.into_iter().map(|s| String::from(s)).collect();
        
        parser.new_node(0, None);
        match parser.parse(&gram, &code) {
            Ok(_) => (),
            Err(_) => (),
        };
    }
}
