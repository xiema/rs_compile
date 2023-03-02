mod tokenizer;
mod grammar;
mod parser;

use std::env;
use std::fs;
use grammar::GrammarGenerator;
use tokenizer::Tokenizer;
use grammar::Grammar;
use parser::{Parser, ParserLL};
// use regex::Regex;

fn lang_tokenizer() -> Tokenizer {
    Tokenizer::new(
        "->|[[:^space:]]+", 
        "[[:space:]&&[^\n]]+", 
        "\n+",
        ";")
}

fn lang_grammar() -> Grammar {
    let mut gram_gen = GrammarGenerator::new();

    gram_gen.new_nonterm("Language");
    gram_gen.new_nonterm("Rule");
    gram_gen.new_nonterm("RHS_Tail");
    let rule_tail = gram_gen.new_nonterm("Rule_Tail");
    gram_gen.new_term("Identifier", "[[^:space:]&&[^;]]");
    gram_gen.new_term("Production_Symbol", "->");
    gram_gen.new_term("EndLine", ";");

    gram_gen.make_prod("Language", vec!["Rule", "Rule_Tail"]);
    gram_gen.make_prod("Rule_Tail", vec!["Rule", "Rule_Tail"]);
    gram_gen.make_eps(rule_tail);
    gram_gen.make_prod("Rule", vec!["Identifier", "Production_Symbol", "Identifier", "RHS_Tail"]);
    gram_gen.make_prod("RHS_Tail", vec!["Identifier", "RHS_Tail"]);
    gram_gen.make_prod("RHS_Tail", vec!["EndLine"]);
    
    gram_gen.generate()
}


fn main() {
    let args: Vec<String> = env::args().collect();

    let asm = match fs::read_to_string(&args[1]) {
        Ok(s) => s,
        Err(e) => panic!("Error: {:?}", e),
    };

    let mut tokenizer = lang_tokenizer();
    let gram = lang_grammar();
    let mut parser = ParserLL::new();

    let tokens = tokenizer.tokenize(asm);
    match parser.parse(&gram, &tokens) {
        Ok(_) => (),
        Err(_) => (),
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lang_test() {
        let mut tok = lang_tokenizer();

        let code = "   \nlhs1 -> rhs1_1 rhs1_2 rhs1_3\n\n  \n lhs2 -> rhs2_1\nlhs3 -> rhs3_1 rhs3_2";
        let tokens = tok.tokenize(String::from(code));

        assert_eq!(tokens[0..6], ["lhs1", "->", "rhs1_1", "rhs1_2", "rhs1_3", ";"]);
        assert_eq!(tokens[6..10], ["lhs2", "->", "rhs2_1", ";"]);
        assert_eq!(tokens[10..15], ["lhs3", "->", "rhs3_1", "rhs3_2", ";"]);
        // tokenizer::display_tokens(&tokens);

        let gram = lang_grammar();
        let mut parser = ParserLL::new();

        parser.new_node(0, None);
        match parser.parse(&gram, &tokens) {
            Ok(_) => (),
            Err(e) => panic!("{}", e),
        }

        // parser::display_ast(0, &parser, &gram, 0);
    }
}