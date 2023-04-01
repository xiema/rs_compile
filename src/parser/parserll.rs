use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::swap;

use anyhow::{anyhow, Context};
use::anyhow::{Result};

use crate::grammar::{Grammar, ProductionId, ElementId, ElementType, Production, GrammarGenerator, ProductionItem};
use crate::tokenizer::{Token, TokenTypeId};

use super::{Node, NodeId, Parser};

pub struct ParserLL {
    pub grammar: Grammar,
    parse_table: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>,
    lookahead: usize,
}

impl ParserLL {
    pub fn new(grammar: &Grammar) -> Self {
        if !grammar.is_parseable_ll() {
            panic!("ParserLL can't parse grammar");
        }
        let grammar = Self::concrete_from(grammar);

        let table = Self::get_parse_table(&grammar);
        let n = table.iter().fold(0, 
                |acc, x| 
                    std::cmp::max(acc, x.iter().fold(0,
                        |acc, x| 
                            std::cmp::max(acc, x.0))));

        Self {
            grammar: grammar,
            parse_table: table,
            lookahead: n,
        }
    }

    fn concrete_from(grammar: &Grammar) -> Grammar {
        let mut gen = GrammarGenerator::new();
        gen.copy_grammar(grammar);
        
        for i in 0..grammar.elems.len() {
            for j in 0..grammar.elems[i].productions.len() {
                for itm_id in 0..grammar.elems[i].productions[j].len() {
                    let itm = &grammar.elems[i].productions[j][itm_id];
                    if itm.kleene_closure {
                        let new_elem_name = format!("{}*", grammar.elems[itm.elem_id].name);
                        let new_id = if gen.elem_map.contains_key(&new_elem_name) {
                            gen.elem_map[&new_elem_name]
                        }
                        else {
                            gen.new_elem(&new_elem_name, ElementType::NonTerminal, true)
                        };
    
                        gen.new_prod(new_id, vec![ProductionItem::new(new_id, false), ProductionItem::new(itm.elem_id, false)]);
                        gen.new_prod(new_id, vec![ProductionItem::new(itm.elem_id, false)]);
                        gen.elems[i].productions[j][itm_id].elem_id = new_id;
                    }
                }
            }
        }

        gen.generate_ll()
    }

    /// Creates a new node, optionally associating it with a parent.
    pub fn new_node(&self, new_node_id: NodeId, elem_id: ElementId, parent: Option<NodeId>) -> Node {
        Node {
            id: new_node_id,
            elem_id: elem_id,
            prod_id: None,
            token: None,
            parent: parent,
            children: Vec::new(),
        }
    }

    /// Create a LL Parse Table
    /// Creates Maps of TokenTypeIds to ProductionIds for each Element with the minimum possible lookaheads
    /// Lookahead positions are determined (and may be sparse), and a Map is created for each position.
    fn get_parse_table(grammar: &Grammar) -> Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>  {
        let mut table: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>> = Vec::new();

        for elem_id in 0..grammar.elems.len() {
            // println!("Finding ProdMap for {}", &grammar.elems[elem_id].name);

            let mut prod_map = Vec::new();

            if grammar.elems[elem_id].productions.len() == 1 {
                table.push(prod_map);
                continue;
            }
            
            // Tuple of (Production Id, ProductionItem Vector, Parent Node Element)
            // ProductionItems and ProductionIds are inspected to find if a unique token 
            //   can be found at a particular lookahead position.
            // Parent Node Element is used to extend the ProductionItem Vector if the lookahead exceeds the current length
            let mut s1: HashSet<(ProductionId, Production, ElementId)> = grammar.elems[elem_id].productions.iter().enumerate().map(
                |(i, p)| (i, p.clone(), elem_id)).collect();
            let mut s2: HashSet<(ProductionId, Production, ElementId)> = HashSet::new();
            let mut lookahead = 1;

            while !s1.is_empty() {
                
                // ensure at least one terminal at front of every rhs
                loop {
                    let mut modified = false;

                    for (prod_id, rhs, id_after) in &s1 {
                        if rhs.len() == 0 {
                            // replace empty rhs with follow set
                            for (list, new_id_after) in &grammar.elems[*id_after].follow_set {
                                s2.insert((*prod_id, list.clone(), *new_id_after));
                                modified = true;
                            }
                        }
                        else {
                            match grammar.elems[rhs[0].elem_id].elem_type {
                                ElementType::NonTerminal => {
                                    // replace nonterminals at front of rhs with corresponding productions
                                    for sub_prod in &grammar.elems[rhs[0].elem_id].productions {
                                        let mut new_prod = sub_prod.clone();
                                        new_prod.extend_from_slice(&rhs[1..]);
                                        s2.insert((*prod_id, new_prod, *id_after));
                                    }
                                    modified = true;
                                },
                                ElementType::Terminal(_) => {
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
                let mut terminal_to_prod: HashMap<ElementId, Vec<ProductionId>> = HashMap::new();
                for (prod_id, rhs, _) in &s1 {
                    if !terminal_to_prod.contains_key(&rhs[0].elem_id) {
                        terminal_to_prod.insert(rhs[0].elem_id, Vec::new());
                    }
                    if !terminal_to_prod[&rhs[0].elem_id].contains(prod_id) {
                        terminal_to_prod.get_mut(&rhs[0].elem_id).unwrap().push(*prod_id);
                    }
                }

                // possible prod map at this lookahead position
                let mut new_map: HashMap<TokenTypeId, ProductionId> = HashMap::new();
                for (elem_id, prod_ids) in &terminal_to_prod {
                    // skip if more than 1 production possible with this token at this position
                    if prod_ids.len() > 1 { continue; }

                    // add this token-production pair to prod_map at this lookahead position
                    let prod_id = prod_ids[0];
                    if let ElementType::Terminal(token_id) = grammar.elems[*elem_id].elem_type {
                        new_map.insert(token_id, prod_id);
    
                        // remove all similar productions (same lookahead token and production id, but possibly different production trees/routes)
                        s1.retain(|(id, p, _)|
                            *id != prod_id
                            || match grammar.elems[p[0].elem_id].elem_type {
                                ElementType::Terminal(t) => t != token_id,
                                ElementType::NonTerminal => true,
                            }
                        );
                    }
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

    /// Uses the parse table to find which production to use for a Element given a sequence of
    /// input tokens.
    fn find_next(&self, elem_id: ElementId, tokens: &[Token]) -> Result<ProductionId> {
        let elem = &self.grammar.elems[elem_id];

        if elem.productions.len() == 1 { return Ok(0); }

        if tokens.len() == 0 { return Err(anyhow!("Tokens length is 0!")) }
        for (i, token) in tokens.iter().enumerate() {
            for (lookahead, map) in &self.parse_table[elem_id] {
                if *lookahead > i + 1 { break; }
                if *lookahead == i + 1 && map.contains_key(&token.token_type) {
                    return Ok(map[&token.token_type]);
                }
            }
        }
        return Err(anyhow!("Couldn't find production for {}", elem.name));
    }

    pub fn display_parse_table(&self) {
        for (i, elem) in self.grammar.elems.iter().enumerate() {
            if self.parse_table[i].len() > 0 {
                println!("{}", elem.name);
                for (lookahead, map) in &self.parse_table[i] {
                    for (tok_id, prod_id) in map {
                        print!("  ({}) {} = ", lookahead, self.grammar.elems[self.grammar.token_elem_map[tok_id]].name);
                        for itm in &elem.productions[*prod_id] {
                            print!("{} ", self.grammar.elems[itm.elem_id].name);
                        }
                        print!("\n");
                    }
                }
            }
        }
    }
}

impl Parser for ParserLL {
    fn parse(&self, tokens: &Vec<Token>) -> Result<Vec<Node>> {
        let mut nodes: Vec<Node> = Vec::new();
        let mut stk: VecDeque<NodeId> = VecDeque::new();
        let mut pos = 0;

        // Create and push ROOT
        nodes.push(self.new_node(0, 0, None));
        stk.push_front(0);

        let mut token = tokens.get(pos).with_context(|| "Empty token sequence")?;

        loop {
            match stk.pop_front() {
                None => break Ok(()),
                Some(cur_node_id) => {
                    let elem = &self.grammar.elems[nodes[cur_node_id].elem_id];
                    token = match tokens.get(pos) {
                        Some(t) => t,
                        None => break Err(anyhow!("Missing tokens at {}:{}, expected {}", token.line_num, token.line_pos, elem.name))
                    };

                    match elem.elem_type {
                        ElementType::Terminal(token_type_id) => {
                            if token_type_id == token.token_type {
                                pos += 1;
                                nodes[cur_node_id].token = Some(token.clone());
                            }
                            else {
                                break Err(anyhow!("Invalid token, found '{}', expected {}", token.text, elem.name))
                            }
                        },
                        ElementType::NonTerminal => {
                            let prod_id = self.find_next(nodes[cur_node_id].elem_id, &tokens[pos..])?;
                    
                            // store production produced by this nonterm
                            nodes[cur_node_id].prod_id = Some(prod_id);

                            for itm in &elem.productions[prod_id] {
                                let new_node_id = nodes.len();
                                // create new node
                                nodes.push(self.new_node(new_node_id, itm.elem_id, Some(cur_node_id)));
                                // associate new node as child of parent node
                                nodes[cur_node_id].children.push(new_node_id);
                            }
                            // push new nodes onto stack
                            for child_id in nodes[cur_node_id].children.iter().rev() {
                                stk.push_front(*child_id);
                            }
                        },
                    }
                }
            }
        }.with_context(|| format!("Parser error at {}:{}", token.line_num, token.line_pos))?;

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

    fn create_lang_simple() -> (Tokenizer, Grammar) {
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
        gram_gen.new_nonterm("Statement");
        gram_gen.new_nonterm("Expression");
        gram_gen.new_term("Term", 0 as TokenTypeId);
        gram_gen.new_term("Operator", 1 as TokenTypeId);
        gram_gen.new_term("EOF", -1 as TokenTypeId);

        gram_gen.make_prod2("Program", vec![("Statement", true), ("EOF", false)]);
        gram_gen.make_prod("Statement", vec!["Expression"]);
        gram_gen.make_prod("Expression", vec!["Expression", "Operator", "Term"]);
        gram_gen.make_prod("Expression", vec!["Term"]);

        let gram = gram_gen.generate();

        (tokenizer, gram)
    }

    fn create_lang_from_lr() -> (Tokenizer, Grammar) {
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
        gram_gen.new_nonterm("Expression_List");
        gram_gen.new_nonterm("Expression");
        gram_gen.new_term("Term", 0 as TokenTypeId);
        gram_gen.new_term("Operator", 1 as TokenTypeId);
        gram_gen.new_term("EOF", -1 as TokenTypeId);

        gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        gram_gen.make_prod("Expression_List", vec!["Expression_List", "Expression"]);
        gram_gen.make_prod("Expression_List", vec!["Expression"]);
        gram_gen.make_prod("Expression", vec!["Expression", "Operator", "Term"]);
        gram_gen.make_prod("Expression", vec!["Term"]);

        let gram = gram_gen.generate_ll();

        (tokenizer, gram)
    }

    #[allow(unused_variables)]
    #[test]
    fn parserll_test() {
    }

    #[allow(unused_variables)]
    #[test]
    fn parserll_from_lr_test() {
        let code = "\n1 + 1\n\n2 + 2\n\n3 + 1 + 2 +2";
        
        let (mut tokenizer, gram) = create_lang_from_lr();
        let tokens = tokenizer.tokenize(code).unwrap();
        let parser = ParserLL::new(&gram);
        let nodes = parser.parse(&tokens).unwrap();
        // println!("{}", gram);
        // display_tree(0, &nodes, &gram, 0);
    }

    #[allow(unused_variables)]
    #[test]
    fn parserll_err_test() {
        let (mut tokenizer, gram) = create_lang_simple();
        let parser = ParserLL::new(&gram);
        
        // parser.display_parse_table();
        
        let code = "1 + 1 \n 1+ +";
        let tokens = tokenizer.tokenize(code).unwrap();
        let res = parser.parse(&tokens);
        let e = res.err().unwrap();
        println!("[DISPLAY] {:#}", e);

        let code = "1 + 1 +";
        let tokens = tokenizer.tokenize(code).unwrap();
        let res = parser.parse(&tokens);
        let e = res.err().unwrap();
        println!("[DISPLAY] {:#}", e);

        let tokens = vec![];
        let res = parser.parse(&tokens);
        let e = res.err().unwrap();
        println!("[DISPLAY] {:#}", e);

        // display_tree(0, &res.unwrap(), &gram, 0);
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
        let nodes = parser.parse(&tokens).unwrap();

        // display_tree(0, &nodes, &gram, 0);
    }
}
