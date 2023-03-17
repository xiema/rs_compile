pub mod tokenizer;
pub mod grammar;
pub mod parser;

use grammar::GrammarGenerator;
use tokenizer::{TokenTypeId, TokenPattern};
use tokenizer::Tokenizer;
use grammar::Grammar;

fn create_lang_gen() -> (Tokenizer, GrammarGenerator) {
    let tokenizer = Tokenizer::new( vec![
        // Tokens
        // Production Symbol
        TokenPattern::Single("->"),
        // Symbol/Identifier
        TokenPattern::Single("[[:word:]]+"),
        // Comment
        TokenPattern::Surround("//", "\n|$", ""),
        // EndRule
        TokenPattern::Single("\n+[[:space:]]*")
    ],
    // Ignore characters
    TokenPattern::Single("[[:space:]]+"),
    // Preprocessor
    Some(|s| String::from(s.trim()) + "\n")
    );

    let mut gram_gen = GrammarGenerator::new();

    gram_gen.new_nonterm("Language");
    gram_gen.new_nonterm("Rule_List");
    gram_gen.new_nonterm("Rule");
    gram_gen.new_nonterm("RHS");
    gram_gen.new_term("Comment", 2 as TokenTypeId);
    gram_gen.new_term("Identifier", 1 as TokenTypeId);
    gram_gen.new_term("Production_Symbol", 0 as TokenTypeId);
    gram_gen.new_term("EndRule", 3 as TokenTypeId);
    gram_gen.new_term("EOF", -1 as TokenTypeId);

    gram_gen.make_prod("Language", vec!["Rule_List", "EOF"]);
    gram_gen.make_prod("Rule_List", vec!["Rule_List", "Rule"]);
    gram_gen.make_prod("Rule_List", vec!["Rule_List", "Comment"]);
    gram_gen.make_prod("Rule_List", vec!["Rule"]);
    gram_gen.make_prod("Rule_List", vec!["Comment"]);
    gram_gen.make_prod("Rule", vec!["Identifier", "Production_Symbol", "RHS", "EndRule"]);
    gram_gen.make_prod("RHS", vec!["RHS", "Identifier"]);
    gram_gen.make_prod("RHS", vec!["Identifier"]);

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
        assert_eq!(parser.get_required_lookahead(), 1);

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

        // parser::display_tree(0, &nodes, &gram, 0);
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

        // grammar::show_follow_sets(&gram.gvars);
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

        let nodes = parser.parse(&tokens).unwrap();

        // parser::display_tree(nodes.len()-1, &nodes, &gram, 0);
    }
}