use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::swap;

use anyhow::{anyhow, Context};
use::anyhow::{Result};

use crate::grammar::{Grammar, ProductionId, GvarId, GvarType};
use crate::tokenizer::{Token, TokenTypeId};

use super::{Node, NodeId, Parser};

pub struct ParserLL {
    grammar: Grammar,
    parse_table: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>,
    lookahead: usize,
}

impl ParserLL {
    pub fn new(grammar: &Grammar) -> Self {
        if !grammar.is_parseable_ll() {
            panic!("ParserLL can't parse grammar");
        }

        let table = Self::get_parse_table(grammar);
        let n = table.iter().fold(0, 
                |acc, x| 
                    std::cmp::max(acc, x.iter().fold(0,
                        |acc, x| 
                            std::cmp::max(acc, x.0))));

        Self {
            grammar: grammar.clone(),
            parse_table: table,
            lookahead: n,
        }
    }

    /// Creates a new node, optionally associating it with a parent.
    pub fn new_node(&self, new_node_id: NodeId, gvar_id: GvarId, parent: Option<NodeId>) -> Node {
        Node {
            id: new_node_id,
            gvar_id: gvar_id,
            prod_id: None,
            token: None,
            parent: parent,
            children: Vec::new(),
        }
    }

    /// Create a LL Parse Table
    /// Creates Maps of TokenTypeIds to ProductionIds for each Gvar with the minimum possible lookaheads
    /// Lookahead positions are determined (and may be sparse), and a Map is created for each position.
    fn get_parse_table(grammar: &Grammar) -> Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>  {
        let mut table: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>> = Vec::new();

        for gvar_id in 0..grammar.gvars.len() {
            // println!("Finding ProdMap for {}", &self.gvars[gvar_id].name);

            let mut prod_map = Vec::new();

            if grammar.gvars[gvar_id].productions.len() == 1 {
                table.push(prod_map);
                continue;
            }
            
            // Tuple of (Production Id, RHS Gvars List, Parent Node Gvar)
            // RHS Gvars (converted to corresponding Tokens) and Production Ids are inspected to find if a unique token 
            //   can be found at a particular lookahead position.
            // Parent Node Gvar is used to extend the RHS Gvar Vector if the lookahead exceeds the current length
            let mut s1: HashSet<(ProductionId, Vec<GvarId>, GvarId)> = grammar.gvars[gvar_id].productions.iter().enumerate().map(|(i, p)| (i, p.clone(), gvar_id)).collect();
            let mut s2: HashSet<(ProductionId, Vec<GvarId>, GvarId)> = HashSet::new();
            let mut lookahead = 1;

            while !s1.is_empty() {
                
                // ensure at least one terminal at front of every rhs
                loop {
                    let mut modified = false;

                    for (prod_id, rhs, id_after) in &s1 {
                        if rhs.len() == 0 {
                            // replace empty rhs with follow set
                            for (list, new_id_after) in &grammar.gvars[*id_after].follow_set {
                                s2.insert((*prod_id, list.clone(), *new_id_after));
                                modified = true;
                            }
                        }
                        else {
                            match grammar.gvars[rhs[0]].gvar_type {
                                GvarType::NonTerminal => {
                                    // replace nonterminals at front of rhs with corresponding productions
                                    for sub_prod in &grammar.gvars[rhs[0]].productions {
                                        let mut new_prod = sub_prod.clone();
                                        new_prod.extend_from_slice(&rhs[1..]);
                                        s2.insert((*prod_id, new_prod, *id_after));
                                    }
                                    modified = true;
                                },
                                GvarType::Terminal => {
                                    // retain terminals
                                    s2.insert((*prod_id, rhs.clone(), *id_after));
                                }
                            }
                        }
                    }

                    swap(&mut s1, &mut s2);
                    s2.clear();
                    if !modified { break; }
                }


                // map of terminals to the possible productions if the terminal is seen at the current position
                let mut terminal_to_prod: HashMap<GvarId, Vec<ProductionId>> = HashMap::new();
                for (prod_id, rhs, _) in &s1 {
                    if !terminal_to_prod.contains_key(&rhs[0]) {
                        terminal_to_prod.insert(rhs[0], Vec::new());
                    }
                    if !terminal_to_prod[&rhs[0]].contains(prod_id) {
                        terminal_to_prod.get_mut(&rhs[0]).unwrap().push(*prod_id);
                    }
                }

                // possible prod map at this lookahead position
                let mut new_map: HashMap<TokenTypeId, ProductionId> = HashMap::new();
                for (gvar_id, prod_ids) in &terminal_to_prod {
                    // skip if more than 1 production possible with this token at this position
                    if prod_ids.len() > 1 { continue; }

                    // add this token-production pair to prod_map at this lookahead position
                    let prod_id = prod_ids[0];
                    let token_id = grammar.gvars[*gvar_id].token_type.unwrap();
                    new_map.insert(token_id, prod_id);

                    // remove all similar productions (same lookahead token and production id, but possibly different production trees/routes)
                    s1.retain(|(id, p, _)|
                        *id != prod_id
                        || grammar.gvars[p[0]].token_type.is_none()
                        || grammar.gvars[p[0]].token_type.unwrap() != token_id
                    );
                }
                
                if !new_map.is_empty() {
                    // add the prod map if something was inserted
                    prod_map.push((lookahead, new_map));
                }

                // advance all rhs lists by 1 token
                for (prod_id, rhs, id_after) in &s1 {
                    s2.insert((*prod_id, Vec::from(&rhs[1..]), *id_after));
                }
                
                swap(&mut s1, &mut s2);
                s2.clear();
                lookahead += 1;
            }

            table.push(prod_map);
        }

        table
    }

    /// Uses the parse table to find which production to use for a Gvar given a sequence of
    /// input tokens.
    fn find_next(&self, gvar_id: GvarId, tokens: &[Token]) -> Result<ProductionId> {
        let gvar = &self.grammar.gvars[gvar_id];

        if gvar.productions.len() == 1 { return Ok(0); }

        if tokens.len() == 0 { return Err(anyhow!("Tokens length is 0!")) }
        for (i, token) in tokens.iter().enumerate() {
            for (lookahead, map) in &self.parse_table[gvar_id] {
                if *lookahead > i + 1 { break; }
                if *lookahead == i + 1 && map.contains_key(&token.token_type) {
                    return Ok(map[&token.token_type]);
                }
            }
        }
        return Err(anyhow!("Couldn't find production for {}", gvar.name));
    }

    pub fn display_parse_table(&self) {
        for (i, gvar) in self.grammar.gvars.iter().enumerate() {
            if self.parse_table[i].len() > 0 {
                println!("{}", gvar.name);
                for (lookahead, map) in &self.parse_table[i] {
                    for (tok_id, prod_id) in map {
                        print!("  ({}) {} = ", lookahead, self.grammar.gvars[self.grammar.token_gvar_map[tok_id]].name);
                        for gvar_id in &gvar.productions[*prod_id] {
                            print!("{} ", self.grammar.gvars[*gvar_id].name);
                        }
                        print!("\n");
                    }
                }
            }
        }
    }
}

impl Parser for ParserLL {
    fn parse(&self, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>> {
        let mut nodes: Vec<Node> = Vec::new();
        let mut stk: VecDeque<NodeId> = VecDeque::new();
        let mut pos = 0;
        nodes.push(self.new_node(0, root, None));
        stk.push_front(0);

        while let Some(cur_node_id) = stk.pop_front() {
            let gvar = &self.grammar.gvars[nodes[cur_node_id].gvar_id];
            tokens.get(pos).with_context(|| format!("Missing tokens at {}, expected {}", pos, gvar.name))?;

            match gvar.gvar_type {
                GvarType::Terminal => {
                    if gvar.token_type.unwrap() == tokens[pos].token_type {
                        nodes[cur_node_id].token = Some(tokens[pos].clone());
                        pos += 1;
                    }
                    else {
                        return Err(anyhow!("Invalid token at {}: found '{}', expected {}", pos, tokens[pos].text, gvar.name));
                    }
                },
                GvarType::NonTerminal => {
                    let prod_id = self.find_next(nodes[cur_node_id].gvar_id, &tokens[pos..])
                        .with_context(|| format!("Parser error at {}: found '{}'", pos, &tokens[pos].text))?;
            
                    // store production produced by this nonterm
                    nodes[cur_node_id].prod_id = Some(prod_id);

                    for child_gvar_id in &gvar.productions[prod_id] {
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

    fn get_required_lookahead(&self) -> usize {
        self.lookahead
    }
}

#[cfg(test)]
mod tests {
    use crate::grammar::*;
    use crate::parser::*;
    use crate::tokenizer::*;

    use super::*;

    fn create_lang() -> (Tokenizer, Grammar) {
        let tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("[0-9]+"),
            TokenPattern::Single("[-+*/]")
        ],
            TokenPattern::Single("[[:space:]]"),
            None
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

        (tokenizer, gram)
    }

    #[allow(unused_variables)]
    #[test]
    fn parserll_test() {
        let (mut tokenizer, gram) = create_lang();

        // println!("{}", gram);

        let code = "\n1 + 1\n\n2 + 2\n\n3 + 1 + 2 +2";
        let tokens = tokenizer.tokenize(code).unwrap();
        
        let parser = ParserLL::new(&gram);
        let nodes = parser.parse(&tokens, 0).unwrap();

        // display_ast(0, &nodes, &gram, 0);
    }

    #[allow(unused_variables)]
    #[test]
    fn parserll_err_test() {
        let (mut tokenizer, gram) = create_lang();
        let parser = ParserLL::new(&gram);
        
        // parser.display_parse_table();
        
        let code = "1 + 1 + +";
        let tokens = tokenizer.tokenize(code).unwrap();
        let res = parser.parse(&tokens, 0);
        let e = res.err().unwrap();
        println!("DisplayErr: {}", e);

        let code = "1 + 1 +";
        let tokens = tokenizer.tokenize(code).unwrap();
        let res = parser.parse(&tokens, 0);
        let e = res.err().unwrap();
        println!("DisplayErr: {}", e);

        // display_ast(0, &res.unwrap(), &gram, 0);
    }

    #[allow(unused_variables)]
    #[test]
    #[should_panic]
    fn lr_to_parserll() {
        let mut tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("[[:digit:]]+"),
            TokenPattern::Single("[-+*/]")
        ],
            TokenPattern::Single("[[:space:]]"),
            None
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
        
        let parser = ParserLL::new(&gram);
        let nodes = parser.parse(&tokens, 0).unwrap();

        // display_ast(0, &nodes, &gram, 0);
    }
}
