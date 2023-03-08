pub mod tokenizer;
pub mod grammar;
pub mod parser;

use grammar::GrammarGenerator;
use tokenizer::TokenTypeId;
use tokenizer::Tokenizer;
use grammar::Grammar;


// Define the tokenizer and meta-grammar that can be used to parse a BNF grammar
pub fn define_lang() -> (Tokenizer, Grammar) {
    let tok = Tokenizer::new( vec![
    // Tokens
        // Production Symbol
            "->",
        // Symbol/Identifier
            "[[:word:]]+",
    ],
    // Ignore characters
    "[[:space:]]+");

    let mut gram_gen = GrammarGenerator::new();

    gram_gen.new_nonterm("Language");
    gram_gen.new_nonterm("Rule_List");
    gram_gen.new_nonterm("Rule_List_Tail");
    gram_gen.new_nonterm("Rule");
    gram_gen.new_nonterm("RHS_Tail");
    gram_gen.new_term("Identifier", 1 as TokenTypeId);
    gram_gen.new_term("Production_Symbol", 0 as TokenTypeId);
    gram_gen.new_term("EOF", -1 as TokenTypeId);

    gram_gen.make_prod("Language", vec!["Rule_List", "EOF"]);
    gram_gen.make_prod("Rule_List", vec!["Rule", "Rule_List_Tail"]);
    gram_gen.make_prod("Rule_List_Tail", vec!["Rule", "Rule_List_Tail"]);
    gram_gen.make_eps("Rule_List_Tail");
    gram_gen.make_prod("Rule", vec!["Identifier", "Production_Symbol", "Identifier", "RHS_Tail"]);
    gram_gen.make_prod("RHS_Tail", vec!["Identifier", "RHS_Tail"]);
    gram_gen.make_eps("RHS_Tail");
    
    let gram = gram_gen.generate();

    return (tok, gram);
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::*;
    use crate::grammar::*;
    use crate::parser::*;

    fn assert_tokens_str(t: &[Token], ts: Vec<&str>) {
        assert_eq!(t.len(), ts.len());
        for i in 0..t.len() {
            assert_eq!(t[i].text, ts[i]);
        }
    }

    #[allow(unused_variables)]
    #[test]
    fn lang_test() {
        let (mut tok, gram) = define_lang();
        let mut parser = ParserLL::new();

        // grammar::show_follow_sets(&gram.gvars);
        // grammar::show_prod_maps(&gram.gvars);

        assert_eq!(gram.class, GrammarClass::LL(2));

        let code = "\n   \n \n\nlhs1 -> rhs1_1 rhs1_2 rhs1_3\n\n  \n lhs2 -> rhs2_1\nlhs3 -> rhs3_1 rhs3_2";
        let tokens = tok.tokenize(code).unwrap();
        
        assert_tokens_str(&tokens[0..5], vec!["lhs1", "->", "rhs1_1", "rhs1_2", "rhs1_3"]);
        assert_tokens_str(&tokens[5..8], vec!["lhs2", "->", "rhs2_1"]);
        assert_tokens_str(&tokens[8..12], vec!["lhs3", "->", "rhs3_1", "rhs3_2"]);
        assert_eq!(tokens[12].token_type, -1);
        // tokenizer::display_tokens(&tokens);

        let nodes = parser.parse(&gram, &tokens, 0).unwrap();

        // parser::display_ast(0, &nodes, &gram, 0);
    }

    #[test]
    #[should_panic]
    fn lang_test_panic() {
        let (mut tok, gram) = define_lang();
        let mut parser = ParserLL::new();
        
        let code = "lhs1 -> rhs1_1 rhs1_2\nlhs2 ->";
        let tokens = tok.tokenize(code).unwrap();
        match parser.parse(&gram, &tokens, 0) {
            Ok(_) => (),
            Err(e) => panic!("{}", e),
        }
    }
}