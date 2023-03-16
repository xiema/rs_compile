use crate::grammar::{GvarId, ProductionId, Grammar};
use crate::tokenizer::{Token};

pub mod parserll;
pub mod parserlr;

pub use parserll::ParserLL;
pub use parserlr::ParserLR;

type NodeId = usize;

#[allow(dead_code)]
pub struct Node {
    id: NodeId,
    gvar_id: GvarId,
    prod_id: Option<ProductionId>,
    token: Option<Token>,
    parent: Option<NodeId>,
    children: Vec<NodeId>,
}

pub trait Parser {
    fn parse(&self, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>, &str>;
    fn get_required_lookahead(&self) -> usize;
}

#[allow(dead_code)]
pub fn display_ast(node_id: NodeId, nodes: &Vec<Node>, gram: &Grammar, level: usize) {
    let indent = String::from("  ").repeat(level);
    print!("{}{}", indent, gram.gvars[nodes[node_id].gvar_id].name);
    match &nodes[node_id].token {
        Some(t) => println!(" >>> '{}'", t.text),
        None => println!()
    }
    for child in &nodes[node_id].children {
        display_ast(*child, nodes, gram, level + 1);
    }
}


#[cfg(test)]
mod tests {
    use crate::grammar::*;
    use crate::tokenizer::*;

    use super::*;

    #[allow(unused_variables)]
    #[test]
    fn parserll_test() {
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

        // println!("{}", gram);

        let code = "\n1 + 1\n\n2 + 2\n\n3 + 1 + 2 +2";
        let tokens = tokenizer.tokenize(code).unwrap();
        
        let parser = ParserLL::new(&gram);
        let nodes = parser.parse(&tokens, 0).unwrap();

        // display_ast(0, &nodes, &gram, 0);
    }

    #[allow(unused_variables)]
    #[test]
    fn parserlr_test() {
        let mut tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("[[:digit:]]+"),
            TokenPattern::Single("[-+*/]"),
            TokenPattern::Single("[;]"),
        ],
            TokenPattern::Single("[[:space:]]"),
            None
        );

        let mut gram_gen = GrammarGenerator::new();
        
        gram_gen.new_nonterm("Program");
        gram_gen.new_nonterm("Expression_List");
        gram_gen.new_nonterm("Expression");
        gram_gen.new_term("Term", 0 as TokenTypeId);
        gram_gen.new_term("Operator", 1 as TokenTypeId);
        gram_gen.new_term("EndExpression", 2 as TokenTypeId);
        gram_gen.new_term("EOF", -1 as TokenTypeId);

        gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        gram_gen.make_prod("Expression_List", vec!["Expression_List", "Expression", "EndExpression"]);
        gram_gen.make_prod("Expression_List", vec!["Expression", "EndExpression"]);
        gram_gen.make_prod("Expression", vec!["Expression", "Operator", "Term"]);
        gram_gen.make_prod("Expression", vec!["Term"]);

        let gram = gram_gen.generate();

        // println!("{}", gram);

        let code = "\n1 + 1;\n\n2 + 2;\n\n3 + 1 + 2 +2;";
        let tokens = tokenizer.tokenize(code).unwrap();
        
        let parser = ParserLR::new(&gram);
        let nodes = parser.parse(&tokens, 0).unwrap();

        // display_ast(nodes.len()-1, &nodes, &gram, 0);
    }

    #[allow(unused_variables)]
    #[test]
    fn parserlr_endlines_test() {
        let mut tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("[[:digit:]]+"),
            TokenPattern::Single("[-+*/]"),
            TokenPattern::Single("\n+[[:space:]]*"),
        ],
            TokenPattern::Single("[[:space:]]"),
            None
        );

        let mut gram_gen = GrammarGenerator::new();
        
        gram_gen.new_nonterm("Program");
        gram_gen.new_nonterm("Expression_List");
        gram_gen.new_nonterm("Expression");
        gram_gen.new_term("Term", 0 as TokenTypeId);
        gram_gen.new_term("Operator", 1 as TokenTypeId);
        gram_gen.new_term("EndLine", 2 as TokenTypeId);
        gram_gen.new_term("EOF", -1 as TokenTypeId);

        gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        gram_gen.make_prod("Expression_List", vec!["Expression_List", "Expression", "EndLine"]);
        gram_gen.make_prod("Expression_List", vec!["Expression", "EndLine"]);
        gram_gen.make_prod("Expression", vec!["Expression", "Operator", "Term"]);
        gram_gen.make_prod("Expression", vec!["Term"]);

        let gram = gram_gen.generate();

        // println!("{}", gram);

        let code = "1 + 1\n\n2 + 2\n\n3 + 1 + 2 +2\n";
        let tokens = tokenizer.tokenize(code).unwrap();
        
        let parser = ParserLR::new(&gram);
        let nodes = parser.parse(&tokens, 0).unwrap();

        // display_ast(nodes.len()-1, &nodes, &gram, 0);
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
