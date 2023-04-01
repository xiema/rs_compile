use std::collections::{VecDeque, HashMap};

use anyhow::Context;
use crate::grammar::{GrammarGenerator, ProductionItem};
use crate::parser::{ParserLR, Parser};
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
        let tree = self.parser.parse(&tokens).unwrap();

        let mut symbol_table = HashMap::new();
        let mut terminals = Vec::new();

        // TODO: add func to check elem ids without storing local vars
        let elem_id_pattern_rule = self.grammar.elem_id_map["PatternRule"];
        let elem_id_production_rule = self.grammar.elem_id_map["ProductionRule"];
        let elem_id_literal = self.grammar.elem_id_map["LiteralSymbol"];
        let elem_id_kleene = self.grammar.elem_id_map["KleeneClosure"];
        let elem_id_eof_marker = self.grammar.elem_id_map["EOFMarker"];
        let mut gram_gen = GrammarGenerator::new();

        // Root
        let root_sym = tree.get_child("Identifier", tree.root_id).unwrap();
        let root_name = &root_sym.token.as_ref().unwrap().text;
        gram_gen.new_nonterm(&root_name);
        gram_gen.new_term("$$", -1 as TokenTypeId);

        // Token setup
        let mut q = VecDeque::new();
        for node_id in tree.iter_descendants("Rule", tree.root_id) {
            let node = &tree.nodes[tree.nodes[node_id].children[0]];

            // Pattern Rules
            if node.elem_id == elem_id_pattern_rule {
                let lhs = tree.get_child("Identifier", node_id).unwrap();
                let rhs = tree.get_child("LiteralSymbol", node_id).unwrap();
                let name = &lhs.token.as_ref().unwrap().text;
                if !symbol_table.contains_key(name) {
                    symbol_table.insert(name.clone(), 0);
                }

                let tuple = (name.clone(), (&rhs.token.as_ref().unwrap().text).clone());
                if !terminals.contains(&tuple) { terminals.push(tuple) };
            }
            // Production Rules
            else if node.elem_id == elem_id_production_rule {
                let lhs = tree.get_child("Identifier", node_id).unwrap();
                let name = &lhs.token.as_ref().unwrap().text;
                if !symbol_table.contains_key(name) {
                    symbol_table.insert(name.clone(), 0);
                }
                for child_id in tree.iter_descendants("LiteralSymbol", node_id) {
                    let sym_node = &tree.nodes[child_id];
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
        // TODO: Check that all nonterminals have at least 1 production
        for (name, elem_id) in &mut symbol_table {
            if terminals.iter().any(|(a,_)| a == name) {
                *elem_id = gram_gen.new_term(name, token_map[name]);
            }
            else {
                *elem_id = gram_gen.new_nonterm(name);
            }
        }

        for node_id in tree.iter_descendants("ProductionRule", tree.root_id) {
            let lhs = tree.get_child("Identifier", node_id).unwrap();
            let def_elem_name = &lhs.token.as_ref().unwrap().text;
            let def_elem_id = symbol_table[def_elem_name];

            for rhs_node_id in tree.iter_descendants("RHS", node_id) {
                let mut rhs_def = Vec::new();
                // get symbols for this RHS
                for sym_id in tree.iter_descendants("Symbol", rhs_node_id) {
                    let symbol = &tree.nodes[tree.nodes[sym_id].children[0]];
                    let tok_text_full = &symbol.token.as_ref().unwrap().text;

                    // special
                    if symbol.elem_id == elem_id_eof_marker {
                        rhs_def.push((1, false));
                    }
                    else {
                        let (tok_text, kleene) = if symbol.elem_id == elem_id_literal {
                            (&tok_text_full[1..tok_text_full.len()-1], false)
                        }
                        else { // Identifier
                            if tree.nodes[sym_id].children.len() > 1 {
                                // Kleene
                                if tree.nodes[tree.nodes[sym_id].children[1]].elem_id == elem_id_kleene {
                                    (tok_text_full.as_str(), true)
                                }
                                else {
                                    panic!("Expected Kleene closure");
                                }
                            }
                            else {
                                (tok_text_full.as_str(), false)
                            }
                        };

                        rhs_def.push((*symbol_table.get(tok_text).with_context(|| format!("Unknown symbol {}", tok_text)).unwrap(), kleene));
                    }
                }

                gram_gen.new_prod(def_elem_id, rhs_def.iter().map(
                    |(x, k)| ProductionItem {elem_id: *x, kleene_closure: *k}
                ).collect());
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
        // Kleene Closure
        TokenPattern::Single("\\*"),
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
    gram_gen.new_term("KleeneClosure", 8 as TokenTypeId);
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
    gram_gen.make_prod("Symbol", vec!["Identifier", "KleeneClosure"]);
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

        show_follow_sets(&gram.elems);
        // show_prod_maps(&gram.elems);

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

        let tree = parser.parse(&tokens).unwrap();

        // println!("{}", tree);
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

        // show_follow_sets(&gram.elems);
        // show_prod_maps(&gram.elems);

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

        let tree = parser.parse(&tokens).unwrap();

        // println!("{}", tree);
    }
}