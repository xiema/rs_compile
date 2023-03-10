use std::collections::{VecDeque};

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
        let mut basis: Vec<(usize, Vec<(GvarId, ProductionId, usize)>)> = Vec::new();
        let mut closure = Vec::new();
        let mut seq = Vec::new();
        let mut pos = 0;

        // get initial
        closure.push((root, 0, 0));

        loop {
            let node = self.new_node(nodes.len(), grammar.token_gvar_map[&tokens[pos].token_type], None);
            seq.push(node.id);
            nodes.push(node);
            let mut seq_pos = seq.len() - 1;

            loop {

                let mut app = Vec::new();

                closure.retain(|(gvar_id, prod_id, start_pos)| {
                    let id_at = grammar.gvars[*gvar_id].productions[*prod_id][seq_pos - start_pos];
                    match grammar.gvars[id_at].gvar_type {
                        GvarType::Terminal => true,
                        GvarType::NonTerminal => {
                            if basis.len() == 0 || basis.last().unwrap().0 < seq_pos {
                                basis.push((seq_pos, vec![]));
                            }
                            let (_, b) = basis.last_mut().unwrap();

                            let x = (*gvar_id, *prod_id, *start_pos);
                            if !b.contains(&x) {
                                b.push(x);
                            }
                            for id in 0..grammar.gvars[id_at].productions.len() {
                                let x = (id_at, id, seq_pos);
                                if !app.contains(&x) && !b.contains(&x) {
                                    app.push(x);
                                }
                            }
                            false
                        }
                    }
                });

                // prevent infinite left-recursion
                app.retain(|x| !closure.contains(x));

                if app.is_empty() {
                    break;
                }
                else {
                    closure.append(&mut app);
                }
            }

            loop {
                // remove unmatched closures or reduce
                let mut reduce = None;
                closure.retain(|(gvar_id, prod_id, start_pos)| {
                    if reduce.is_some() { return false; }

                    let id = grammar.gvars[*gvar_id].productions[*prod_id][seq_pos-start_pos];
                    match grammar.gvars[id].gvar_type {
                        GvarType::Terminal => {
                            if nodes[seq[seq_pos]].gvar_id == id {
                                if grammar.gvars[*gvar_id].productions[*prod_id].len() - 1 == seq_pos - start_pos {
                                    // println!("Reducing: {} {}", grammar.gvars[*gvar_id].name, prod_id);
                                    reduce = Some((*gvar_id, *prod_id, *start_pos));
                                }
                                return true;
                            }
                            return false;
                        },
                        GvarType::NonTerminal => {
                            if grammar.gvars[id].first_set.contains(&nodes[seq[seq_pos]].gvar_id) {
                                if grammar.gvars[*gvar_id].productions[*prod_id].len() - 1 == seq_pos - start_pos {
                                    // println!("Reducing: {} {}", grammar.gvars[*gvar_id].name, prod_id);
                                    reduce = Some((*gvar_id, *prod_id, *start_pos));
                                }
                                return true;
                            }
                            return false;
                        }
                    }
                });

                if let Some((gvar_id, prod_id, start_pos)) = reduce {
                    loop {
                        if let Some((x, _)) = basis.last() {
                            if *x > start_pos {
                                basis.pop();
                                continue;
                            }
                        }
                        break;
                    }

                    // create nodes
                    let mut node = self.new_node(nodes.len(), gvar_id, None);
                    node.prod_id = Some(prod_id);
                    for id in &seq[start_pos..] {
                        nodes[*id].parent = Some(node.id);
                        node.children.push(*id);
                    }
                    seq.truncate(start_pos);
                    seq.push(node.id);
                    nodes.push(node);
                    seq_pos = seq.len() - 1;


                    // new closure
                    if basis.len() > 0 {
                        closure = basis.last().unwrap().1.clone();
                        
                        // re-add basis
                        closure.retain(|(id, prod_id, start_pos)| {
                            let id_at = grammar.gvars[*id].productions[*prod_id][seq_pos - start_pos];
                            // we are sure it's a nonterm in id_at
                            match grammar.gvars[id_at].gvar_type {
                                GvarType::Terminal => panic!("Terminal at handle of basis"),
                                GvarType::NonTerminal => {
                                    if id_at == gvar_id {
                                        return true;
                                    }
                                    else if grammar.gvars[id_at].first_set.contains(&gvar_id) {
                                        if basis.len() == 0 || basis.last().unwrap().0 < seq_pos {
                                            basis.push((seq_pos, vec![]));
                                        }
                                        let (_, b) = basis.last_mut().unwrap();
    
                                        let x = (*id, *prod_id, *start_pos);
                                        if !b.contains(&x) {
                                            b.push(x);
                                        }                      
                                    }
                                    return false;
                                }
                            }
                        });
                    }

                    // try to reduce again
                    continue;
                }

                break;
            }

            pos += 1;

            if closure.is_empty() { break; }
            if pos > tokens.len() {
                panic!("Missing tokens");
            }
        }

        if pos < tokens.len() {
            panic!("Excess tokens: {}", tokens.len() - pos);
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
