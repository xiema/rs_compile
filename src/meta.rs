use std::collections::{VecDeque, HashMap};

use anyhow::Context;
use crate::grammar::{GrammarGenerator};
use crate::parser::{Node, NodeId, ParserLR, Parser};
use crate::tokenizer::{TokenTypeId, TokenPattern};
use crate::tokenizer::Tokenizer;
use crate::grammar::Grammar;

pub struct GrammarReader {
    tokenizer: Tokenizer,
    parser: ParserLR,
    grammar: Grammar,
}

impl GrammarReader {
    pub fn new() -> Self {
        let (tokenizer, grammar) = define_lang_lr();
        let parser = ParserLR::new(&grammar);
        GrammarReader { tokenizer, parser, grammar }
    }

    pub fn read(&mut self, in_str: &str) -> (Tokenizer, Grammar) {
        let tokens = self.tokenizer.tokenize(in_str).unwrap();
        let nodes = self.parser.parse(&tokens).unwrap();
        let root = nodes.len()-1;

        let mut symbol_table = HashMap::new();
        let mut terminals = Vec::new();
        let gvar_id_pattern_rule = self.grammar.gvar_id_map["PatternRule"];
        let gvar_id_production_rule = self.grammar.gvar_id_map["ProductionRule"];
        let gvar_id_literal = self.grammar.gvar_id_map["LiteralSymbol"];
        let gvar_id_eof_marker = self.grammar.gvar_id_map["EOFMarker"];
        let mut gram_gen = GrammarGenerator::new();

        // Root
        let root_sym = &nodes[get_child("Identifier", root, &nodes, &self.grammar).unwrap()];
        let root_name = &root_sym.token.as_ref().unwrap().text;
        gram_gen.new_nonterm(&root_name);
        gram_gen.new_term("$$", -1 as TokenTypeId);

        // Token setup
        let mut q = VecDeque::new();
        for node_id in iter_descendants("Rule", root, &nodes, &self.grammar) {
            let node = &nodes[nodes[node_id].children[0]];

            if node.gvar_id == gvar_id_pattern_rule {
                let lhs = &nodes[get_child("Identifier", node_id, &nodes, &self.grammar).unwrap()];
                let rhs = &nodes[get_child("LiteralSymbol", node_id, &nodes, &self.grammar).unwrap()];
                let name = &lhs.token.as_ref().unwrap().text;
                if !symbol_table.contains_key(name) {
                    symbol_table.insert(name.clone(), 0);
                }

                let tuple = (name.clone(), (&rhs.token.as_ref().unwrap().text).clone());
                if !terminals.contains(&tuple) { terminals.push(tuple) };
            }
            else if node.gvar_id == gvar_id_production_rule {
                let lhs = &nodes[get_child("Identifier", node_id, &nodes, &self.grammar).unwrap()];
                let name = &lhs.token.as_ref().unwrap().text;
                if !symbol_table.contains_key(name) {
                    symbol_table.insert(name.clone(), 0);
                }
                for child_id in iter_descendants("LiteralSymbol", node_id, &nodes, &self.grammar) {
                    let sym_node = &nodes[child_id];
                    let sym_text_full = &sym_node.token.as_ref().unwrap().text;
                    let sym_text = &sym_text_full[1..sym_text_full.len()-1];
                    if !symbol_table.contains_key(sym_text) {
                        symbol_table.insert(String::from(sym_text), 0);
                        
                        let tuple = (String::from(sym_text), String::from(sym_text_full));
                        if !terminals.contains(&tuple) { terminals.push(tuple) };
                    }
                }
            }
            else {
                for child_id in &node.children {
                    q.push_back(*child_id);
                }
            }
        }

        let mut tokens = Vec::new();
        let mut token_map = HashMap::new();
        for (name, pat) in &terminals {
            token_map.insert(name.clone(), tokens.len() as TokenTypeId);
            tokens.push(TokenPattern::Single(&pat[1..pat.len()-1]))
        }

        let tokenizer = Tokenizer::new(
            tokens,
            TokenPattern::Single("[[:space:]]+"),
            Some(|s| String::from(s.trim()) + "\n")
        );


        // Grammar Setup
        for (name, gvar_id) in &mut symbol_table {
            if terminals.iter().any(|(a,_)| a == name) {
                *gvar_id = gram_gen.new_term(name, token_map[name]);
            }
            else {
                *gvar_id = gram_gen.new_nonterm(name);
            }
        }

        for node_id in iter_descendants("ProductionRule", root, &nodes, &self.grammar) {
            let lhs = &nodes[get_child("Identifier", node_id, &nodes, &self.grammar).unwrap()];
            let def_gvar_name = &lhs.token.as_ref().unwrap().text;
            let def_gvar_id = symbol_table[def_gvar_name];

            for rhs_node_id in iter_descendants("RHS", node_id, &nodes, &self.grammar) {
                let mut rhs_def = Vec::new();
                // get symbols for this RHS
                for sym_id in iter_descendants("Symbol", rhs_node_id, &nodes, &self.grammar) {
                    let symbol = &nodes[nodes[sym_id].children[0]];
                    let tok_text_full = &symbol.token.as_ref().unwrap().text;
                    let tok_text = if symbol.gvar_id == gvar_id_literal {
                        &tok_text_full[1..tok_text_full.len()-1]
                    }
                    else { // Identifier
                        tok_text_full.as_str()
                    };

                    // special
                    if symbol.gvar_id == gvar_id_eof_marker {
                        rhs_def.push(1);
                    }
                    else {
                        rhs_def.push(*symbol_table.get(tok_text).with_context(|| format!("Unknown symbol {}", tok_text)).unwrap());
                    }
                }

                gram_gen.new_prod(def_gvar_id, rhs_def);
            }
        }

        let gram = gram_gen.generate();

        (tokenizer, gram)
    }

}

fn create_lang_gen() -> (Tokenizer, GrammarGenerator) {
    let tokenizer = Tokenizer::new( vec![
        // Tokens
        // Production Symbol
        TokenPattern::Single("->"),
        // Or
        TokenPattern::Single("\\|"),
        // Comment
        TokenPattern::Surround("//", "\n|$", ""),
        // EndRule
        TokenPattern::Single("\n+[[:space:]]*"),
        // EOFMarker
        TokenPattern::Single("\\$\\$"),
        // LiteralSymbol
        TokenPattern::Surround("\"", "\"", "\\\\"),
        // PatternSymbol
        TokenPattern::Single("~="),
        // Symbol/Identifier
        TokenPattern::Single("[[:alnum:]_]+"),
    ],
    // Ignore characters
    TokenPattern::Single("[[:space:]]+"),
    // Preprocessor
    Some(|s| String::from(s.trim()) + "\n")
    );

    let mut gram_gen = GrammarGenerator::new();

    gram_gen.new_nonterm("Grammar");
    gram_gen.new_nonterm("RuleList");
    gram_gen.new_nonterm("Rule");
    gram_gen.new_nonterm("ProductionRule");
    gram_gen.new_nonterm("PatternRule");
    gram_gen.new_nonterm("RHSList");
    gram_gen.new_nonterm("RHS");
    gram_gen.new_nonterm("Symbol");
    gram_gen.new_term("ProductionSymbol", 0 as TokenTypeId);
    gram_gen.new_term("Or", 1 as TokenTypeId);
    gram_gen.new_term("Comment", 2 as TokenTypeId);
    gram_gen.new_term("EndRule", 3 as TokenTypeId);
    gram_gen.new_term("EOFMarker", 4 as TokenTypeId);
    gram_gen.new_term("LiteralSymbol", 5 as TokenTypeId);
    gram_gen.new_term("PatternSymbol", 6 as TokenTypeId);
    gram_gen.new_term("Identifier", 7 as TokenTypeId);
    gram_gen.new_term("$$", -1 as TokenTypeId);

    gram_gen.make_prod("Grammar", vec!["RuleList", "$$"]);
    gram_gen.make_prod("RuleList", vec!["RuleList", "Rule"]);
    gram_gen.make_prod("RuleList", vec!["RuleList", "Comment"]);
    gram_gen.make_prod("RuleList", vec!["Rule"]);
    gram_gen.make_prod("RuleList", vec!["Comment"]);
    gram_gen.make_prod("Rule", vec!["ProductionRule"]);
    gram_gen.make_prod("Rule", vec!["PatternRule"]);
    gram_gen.make_prod("ProductionRule", vec!["Identifier", "ProductionSymbol", "RHSList", "EndRule"]);
    gram_gen.make_prod("PatternRule", vec!["Identifier", "PatternSymbol", "LiteralSymbol", "EndRule"]);
    gram_gen.make_prod("RHSList", vec!["RHS", "Or", "RHSList"]);
    gram_gen.make_prod("RHSList", vec!["RHS"]);
    gram_gen.make_prod("RHS", vec!["RHS", "Symbol"]);
    gram_gen.make_prod("RHS", vec!["Symbol"]);
    gram_gen.make_prod("Symbol", vec!["Identifier"]);
    gram_gen.make_prod("Symbol", vec!["LiteralSymbol"]);
    gram_gen.make_prod("Symbol", vec!["EOFMarker"]);


    (tokenizer, gram_gen)
}


// Define the tokenizer and meta-grammar (as LL) that can be used to parse a BNF grammar
pub fn define_lang_ll() -> (Tokenizer, Grammar) {
    let (tok, mut gram_gen) = create_lang_gen();
    let gram = gram_gen.generate_ll();

    return (tok, gram);
}


// Define the tokenizer and meta-grammar (as LR) that can be used to parse a BNF grammar
pub fn define_lang_lr() -> (Tokenizer, Grammar) {
    let (tok, mut gram_gen) = create_lang_gen();
    let gram = gram_gen.generate();

    return (tok, gram);
}

fn iter_descendants(name: &str, base_node_id: NodeId, nodes: &Vec<Node>, gram: &Grammar) -> Vec<NodeId> {
    let mut q = VecDeque::new();
    let mut ret = Vec::new();
    q.push_front(base_node_id);

    while !q.is_empty() {
        let node_id = q.pop_front().unwrap();
        if gram.gvars[nodes[node_id].gvar_id].name == name {
            ret.push(node_id);
        }
        else {
            for child_id in nodes[node_id].children.iter().rev() {
                q.push_front(*child_id);
            }
        }
    }

    ret
}

fn get_child(name: &str, base_node_id: NodeId, nodes: &Vec<Node>, gram: &Grammar) -> Option<NodeId> {
    let mut q = VecDeque::new();
    q.push_front(base_node_id);

    while !q.is_empty() {
        let node_id = q.pop_front().unwrap();
        if gram.gvars[nodes[node_id].gvar_id].name == name {
            return Some(node_id);
        }
        else {
            for child_id in nodes[node_id].children.iter().rev() {
                q.push_front(*child_id);
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::*;
    use crate::parser::*;
    use crate::grammar::*;

    #[allow(dead_code)]
    fn assert_tokens_str(t: &[Token], ts: Vec<&str>) {
        assert_eq!(t.len(), ts.len());
        for i in 0..t.len() {
            assert_eq!(t[i].text, ts[i]);
        }
    }

    #[allow(unused_variables)]
    #[test]
    fn lang_test_ll() {
        let (mut tok, gram) = define_lang_ll();
        let parser = ParserLL::new(&gram);

        // grammar::show_follow_sets(&gram.gvars);
        // grammar::show_prod_maps(&gram.gvars);

        assert_eq!(gram.is_parseable_ll(), true);
        assert_eq!(parser.get_required_lookahead(), 2);

        let code = "\n   \n \n\nlhs1 -> rhs1_1 rhs1_2 rhs1_3\n\n  \n //comment here  \nlhs2 -> rhs2_1\nlhs3 -> rhs3_1 rhs3_2";
        let tokens = tok.tokenize(code).unwrap();

        let check = vec![
            vec!["lhs1", "->", "rhs1_1", "rhs1_2", "rhs1_3", "\n\n  \n "],
            vec!["//comment here  \n"],
            vec!["lhs2", "->", "rhs2_1", "\n"],
            vec!["lhs3", "->", "rhs3_1", "rhs3_2", "\n"],
        ];

        assert_eq!(tokens.len(), check.iter().fold(0, |acc, v| acc + v.len()) + 1);

        let mut i = 0;
        for v in check {
            for s in v {
                assert_eq!(tokens[i].text, s);
                i+=1;
            }
        }
        assert_eq!(tokens[tokens.len()-1].token_type, -1 as TokenTypeId);

        let nodes = parser.parse(&tokens).unwrap();

        // display_tree(0, &nodes, &gram, 0);
    }

    #[allow(unused_variables)]
    #[test]
    #[should_panic]
    fn lang_test_panic() {
        let (mut tok, gram) = define_lang_ll();
        let parser = ParserLL::new(&gram);
        
        let code = "lhs1 -> rhs1_1 rhs1_2\nlhs2 ->";
        let tokens = tok.tokenize(code).unwrap();
        let nodes = parser.parse(&tokens).unwrap();
    }

    #[allow(unused_variables)]
    #[test]
    fn lang_test_lr() {
        let (mut tok, gram) = define_lang_lr();
        let parser = ParserLR::new(&gram);

        // show_follow_sets(&gram.gvars);
        // grammar::show_prod_maps(&gram.gvars);

        assert_eq!(gram.is_parseable_lr(), true);
        // assert_eq!(parser.get_required_lookahead(), 2);

        let code = "\n   \n \n\nlhs1 -> rhs1_1 rhs1_2 rhs1_3\n\n  \n //comment here  \nlhs2 -> rhs2_1\nlhs3 -> rhs3_1 rhs3_2\n//comment";
        let tokens = tok.tokenize(code).unwrap();

        let check = vec![
            vec!["lhs1", "->", "rhs1_1", "rhs1_2", "rhs1_3", "\n\n  \n "],
            vec!["//comment here  \n"],
            vec!["lhs2", "->", "rhs2_1", "\n"],
            vec!["lhs3", "->", "rhs3_1", "rhs3_2", "\n"],
            vec!["//comment\n"],
        ];

        assert_eq!(tokens.len(), check.iter().fold(0, |acc, v| acc + v.len()) + 1);

        let mut i = 0;
        for v in check {
            for s in v {
                assert_eq!(tokens[i].text, s);
                i+=1;
            }
        }
        assert_eq!(tokens[tokens.len()-1].token_type, -1 as TokenTypeId);

        // parser.display_parse_table();

        let nodes = parser.parse(&tokens).unwrap();

        // parser::display_tree(nodes.len()-1, &nodes, &gram, 0);
    }
}