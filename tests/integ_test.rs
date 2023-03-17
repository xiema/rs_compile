use rs_compile::parser::*;
use std::fs;


#[allow(unused_variables)]
#[test]
fn integ_test_ll() {
    let (mut tok, gram) = rs_compile::define_lang_ll();
    
    let asm = fs::read_to_string("tests/input/sample_grammar_ll.txt").unwrap();
    let tokens = tok.tokenize(asm.as_str()).unwrap();
    let parser = ParserLL::new(&gram);
    let ast = parser.parse(&tokens).unwrap();
    display_ast(0, &ast, &gram, 0);
    
    let asm = fs::read_to_string("tests/input/sample_grammar_lr.txt").unwrap();
    let tokens = tok.tokenize(asm.as_str()).unwrap();
    let parser = ParserLL::new(&gram);
    let ast = parser.parse(&tokens).unwrap();
    display_ast(0, &ast, &gram, 0);

    // parser.display_parse_table();
}

#[allow(unused_variables)]
#[test]
fn integ_test_lr() {
    let (mut tok, gram) = rs_compile::define_lang_lr();

    let asm = fs::read_to_string("tests/input/sample_grammar_lr.txt").unwrap();
    let tokens = tok.tokenize(asm.as_str()).unwrap();
    let parser = ParserLR::new(&gram);
    let ast = parser.parse(&tokens).unwrap();
    display_ast(ast.len()-1, &ast, &gram, 0);

    let asm = fs::read_to_string("tests/input/sample_grammar_ll.txt").unwrap();
    let tokens = tok.tokenize(asm.as_str()).unwrap();
    let parser = ParserLR::new(&gram);
    let ast = parser.parse(&tokens).unwrap();
    display_ast(ast.len()-1, &ast, &gram, 0);

    // parser.display_parse_table();
}
