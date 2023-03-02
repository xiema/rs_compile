mod tokenizer;
mod grammar;
mod parser;

use std::env;
use std::fs;
use grammar::GrammarGenerator;
use tokenizer::TokenTypeId;
use tokenizer::Tokenizer;
use grammar::Grammar;
use parser::{Parser, ParserLL};

fn define_lang() -> (Tokenizer, Grammar) {
    let tok = Tokenizer::new(
        vec!["\n+[[:space:]]*", "->", "[[:^space:]]+"],
        "[[:space:]&&[^\n]]+");

    let mut gram_gen = GrammarGenerator::new();

    gram_gen.new_nonterm("Language");
    gram_gen.new_nonterm("Rule");
    gram_gen.new_nonterm("RHS_Tail");
    let rule_tail = gram_gen.new_nonterm("Rule_Tail");
    gram_gen.new_term("Identifier", 2 as TokenTypeId);
    gram_gen.new_term("Production_Symbol", 1 as TokenTypeId);
    gram_gen.new_term("EndLine", 0 as TokenTypeId);
    gram_gen.new_term("EOF", -1 as TokenTypeId);

    gram_gen.make_prod("Language", vec!["Rule", "Rule_Tail"]);
    gram_gen.make_prod("Language", vec!["EndLine", "Rule", "Rule_Tail"]);
    gram_gen.make_prod("Rule_Tail", vec!["Rule", "Rule_Tail"]);
    gram_gen.make_eps(rule_tail);
    gram_gen.make_prod("Rule", vec!["Identifier", "Production_Symbol", "Identifier", "RHS_Tail"]);
    gram_gen.make_prod("RHS_Tail", vec!["Identifier", "RHS_Tail"]);
    gram_gen.make_prod("RHS_Tail", vec!["EndLine"]);
    gram_gen.make_prod("RHS_Tail", vec!["EOF"]);
    
    let gram = gram_gen.generate();

    return (tok, gram);
}


fn main() {
    let args: Vec<String> = env::args().collect();

    let asm = match fs::read_to_string(&args[1]) {
        Ok(s) => s,
        Err(e) => panic!("Error: {:?}", e),
    };

    let (mut tokenizer, gram) = define_lang();
    let mut parser = ParserLL::new();

    let tokens = tokenizer.tokenize(asm.as_str());
    match parser.parse(&gram, &tokens) {
        Ok(_) => (),
        Err(_) => (),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::*;

    fn assert_tokens_str(t: &[Token], ts: Vec<&str>) {
        assert_eq!(t.len(), ts.len());
        for i in 0..t.len() {
            assert_eq!(t[i].text, ts[i]);
        }
    }

    #[test]
    fn lang_test() {
        let (mut tok, gram) = define_lang();
        let mut parser = ParserLL::new();

        let code = "\n   \n \n\nlhs1 -> rhs1_1 rhs1_2 rhs1_3\n\n  \n lhs2 -> rhs2_1\nlhs3 -> rhs3_1 rhs3_2";
        let tokens = tok.tokenize(code);
        
        assert_tokens_str(&tokens[0..5], vec!["lhs1", "->", "rhs1_1", "rhs1_2", "rhs1_3"]);
        assert_eq!(tokens[5].token_type, 0 as TokenTypeId);
        assert_tokens_str(&tokens[6..9], vec!["lhs2", "->", "rhs2_1"]);
        assert_eq!(tokens[9].token_type, 0 as TokenTypeId);
        assert_tokens_str(&tokens[10..14], vec!["lhs3", "->", "rhs3_1", "rhs3_2"]);
        assert_eq!(tokens[14].token_type, -1);
        // tokenizer::display_tokens(&tokens);

        parser.new_node(0, None);
        match parser.parse(&gram, &tokens) {
            Ok(_) => (),
            Err(e) => panic!("{}", e),
        }

        parser::display_ast(0, &parser, &gram, 0);
    }

    #[test]
    #[should_panic]
    fn lang_test_panic() {
        let (mut tok, gram) = define_lang();
        let mut parser = ParserLL::new();
        
        let code = "lhs1 -> rhs1_1 rhs1_2\nlhs2 ->";
        let tokens = tok.tokenize(code);
        parser.new_node(0, None);
        match parser.parse(&gram, &tokens) {
            Ok(_) => (),
            Err(e) => panic!("{}", e),
        }
    }
}