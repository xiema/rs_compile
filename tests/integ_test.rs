use rs_compile::{parser::*, meta::*};
use std::fs;


#[allow(unused_variables)]
#[test]
fn integ_test_ll() {
    let (mut tok, gram) = define_lang_ll();
    
    let gram_txt = fs::read_to_string("tests/input/sample_grammar_ll.txt").unwrap();
    let tokens = tok.tokenize(gram_txt.as_str()).unwrap();
    let parser = ParserLL::new(&gram);
    let tree = parser.parse(&tokens).unwrap();
    display_tree(0, &tree, &gram, 0);
    
    let gram_txt = fs::read_to_string("tests/input/sample_grammar_lr.txt").unwrap();
    let tokens = tok.tokenize(gram_txt.as_str()).unwrap();
    let parser = ParserLL::new(&gram);
    let tree = parser.parse(&tokens).unwrap();
    display_tree(0, &tree, &gram, 0);

    // parser.display_parse_table();
}

#[allow(unused_variables)]
#[test]
fn integ_test_lr() {
    let (mut tok, gram) = define_lang_lr();

    let gram_txt = fs::read_to_string("tests/input/sample_grammar_lr.txt").unwrap();
    let tokens = tok.tokenize(gram_txt.as_str()).unwrap();
    let parser = ParserLR::new(&gram);
    let tree = parser.parse(&tokens).unwrap();
    display_tree(tree.len()-1, &tree, &gram, 0);

    let gram_txt = fs::read_to_string("tests/input/sample_grammar_ll.txt").unwrap();
    let tokens = tok.tokenize(gram_txt.as_str()).unwrap();
    let parser = ParserLR::new(&gram);
    let tree = parser.parse(&tokens).unwrap();
    display_tree(tree.len()-1, &tree, &gram, 0);

    // parser.display_parse_table();
}

#[allow(unused_variables, unused_mut)]
#[test]
fn integ_test_meta() {
    let mut reader = GrammarReader::new();

    let gram_txt = fs::read_to_string("tests/input/sample_grammar_lr.txt").unwrap();
    let (mut tok, gram) = reader.read(gram_txt.as_str());
    let parser = ParserLR::new(&gram);

    let prog_txt = fs::read_to_string("tests/input/lr_sample.in").unwrap();
    let tokens = tok.tokenize(prog_txt.as_str()).unwrap();
    let tree = parser.parse(&tokens).unwrap();
    display_tree(tree.len()-1, &tree, &gram, 0);
}