use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result};

use crate::grammar::{Grammar, ProductionId, GvarId, GvarType};
use crate::tokenizer::{Token, TokenTypeId};

use super::*;

pub enum ParseAction {
    Shift(usize),
    Reduce(GvarId, ProductionId),
    ShiftReduce(GvarId, ProductionId),
}

pub struct ParserLR {
    grammar: Grammar,
    parse_table: Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>>,
    state_defs: Vec<HashSet<(GvarId, ProductionId, usize)>>,
    lookahead: usize,
}

pub enum ParserLRErr {
    UnknownStateShift(usize, usize, TokenTypeId),
}

impl ParserLR {
    pub fn new(grammar: &Grammar) -> Self {
        let (state_defs, table) = Self::get_parse_table(grammar);
        let n = table.iter().fold(0, 
                |acc, x| 
                    std::cmp::max(acc, x.iter().fold(0,
                        |acc, x| 
                            std::cmp::max(acc, x.0))));

        Self {
            grammar: grammar.clone(),
            parse_table: table,
            state_defs: state_defs,
            lookahead: n,
        }
    }

    /// Creates a new node, optionally associating it with a parent.
    pub fn new_node(&self, new_node_id: NodeId, gvar_id: GvarId, parent: Option<NodeId>) -> Node {
        Node {
            id: new_node_id,
            gvar_id: gvar_id,
            prod_id: None,
            token: None,
            parent: parent,
            children: Vec::new(),
        }
    }

    /// Creates a LR Parse Table
    /// Creates Maps of Terminal GvarIds to ParseActions for each lookahead position.
    /// Currently only creates 1 Map at lookahead=1.
    fn get_parse_table(grammar: &Grammar) -> (Vec<HashSet<(GvarId, ProductionId, usize)>>, Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>>) {
        let mut table: Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>> = Vec::new();
        let mut state_defs: Vec<HashSet<(GvarId, ProductionId, usize)>> = Vec::new();

        let mut cur_state_id = 0;
        
        // A State is composed of Substates which are tuples of
        //   (GvarId, ProductionId, ProductionPosition)
        //   GvarId: Which Gvar at the LHS
        //   ProductionId: Which Production of GvarId at the RHS
        //   ProductionPosition: The handle position in the RHS
        let mut starting_state = HashSet::new();
        starting_state.insert((0, 0, 0));
        state_defs.push(starting_state);

        while cur_state_id < state_defs.len() {
            let mut closures = HashSet::new();
            let mut follow_ids = HashSet::new();
            let mut action_map: HashMap<GvarId, ParseAction> = HashMap::new();

            // get all closures and follow ids

            // get starting closures and follow ids
            for (gvar_id, prod_id, prod_pos) in &state_defs[cur_state_id] {
                if *prod_pos < grammar.gvars[*gvar_id].productions[*prod_id].len() {
                    let id = grammar.gvars[*gvar_id].productions[*prod_id][*prod_pos];
                    if matches!(grammar.gvars[id].gvar_type, GvarType::NonTerminal) {
                        for i in 0..grammar.gvars[id].productions.len() {
                            closures.insert((id, i));
                        }
                    }
                    follow_ids.insert(id);
                }
            }
            // get derivative closures and follow ids
            loop {
                let mut new_closures = HashSet::new();
                for (gvar_id, prod_id) in &closures {
                    let id = grammar.gvars[*gvar_id].productions[*prod_id][0];
                    if matches!(grammar.gvars[id].gvar_type, GvarType::NonTerminal) {
                        for i in 0..grammar.gvars[id].productions.len() {
                            let new_closure = (id, i);
                            if !closures.contains(&new_closure) {
                                new_closures.insert(new_closure);
                            }
                        }
                    }
                    follow_ids.insert(id);
                }
                if new_closures.is_empty() { break; }
                closures.extend(new_closures);
            }

            // if handle at end of any basis, add REDUCE action
            let mut seen = HashSet::new();
            for (gvar_id, prod_id, prod_pos) in &state_defs[cur_state_id] {
                if *prod_pos == grammar.gvars[*gvar_id].productions[*prod_id].len() {
                    for (rhs, _) in &grammar.gvars[*gvar_id].follow_set {
                        if follow_ids.contains(&rhs[0]) || seen.contains(&rhs[0]) {
                            panic!("Grammar is not LR(1)");
                        }
                        action_map.insert(rhs[0], ParseAction::Reduce(*gvar_id, *prod_id));
                        seen.insert(rhs[0]);
                    }
                }
            }

            // actions for each follow id
            for follow_id in follow_ids {
                let mut new_state = HashSet::new();
                let mut reduce = false;
                
                // bases
                for (gvar_id, prod_id, prod_pos) in &state_defs[cur_state_id] {
                    if *prod_pos < grammar.gvars[*gvar_id].productions[*prod_id].len() {
                        let id = grammar.gvars[*gvar_id].productions[*prod_id][*prod_pos];
                        if id == follow_id {
                            new_state.insert((*gvar_id, *prod_id, prod_pos + 1));
                            reduce |= prod_pos + 1 == grammar.gvars[*gvar_id].productions[*prod_id].len();
                        }
                    }
                }
                // closures
                for (gvar_id, prod_id) in &closures {
                    let id = grammar.gvars[*gvar_id].productions[*prod_id][0];
                    if id == follow_id {
                        new_state.insert((*gvar_id, *prod_id, 1));
                        reduce |= 1 == grammar.gvars[*gvar_id].productions[*prod_id].len();
                    }
                }

                if new_state.len() == 1 && reduce {
                    // SHIFT-REDUCE
                    // only if there is a unique substate to reduce
                    for (gvar_id, prod_id, _) in &new_state {
                        action_map.insert(follow_id, ParseAction::ShiftReduce(*gvar_id, *prod_id));
                    }
                }
                else {
                    // SHIFT
                    // add the new state if it is unique, or else get the existing state_id
                    let next_state_id = match state_defs.iter().enumerate()
                        .find(|(_, state_def)| 
                            new_state.eq(&state_def) && state_def.eq(&&new_state)
                    ) {
                        None => {
                            state_defs.push(new_state);
                            state_defs.len() - 1
                        },
                        Some((id, _)) => {
                            id
                        }
                    };
                    action_map.insert(follow_id, ParseAction::Shift(next_state_id));
                }
            }

            table.push(vec![(1, action_map)]);

            cur_state_id += 1;
        }

        (state_defs, table)
    }

    pub fn display_parse_table(&self) {
        for (i, state_def) in self.state_defs.iter().enumerate() {
            println!("State {}:", i);

            for (gvar_id, prod_id, prod_pos) in state_def {
                print!("  {} --> ", self.grammar.gvars[*gvar_id].name);
                for (j, id) in self.grammar.gvars[*gvar_id].productions[*prod_id].iter().enumerate() {
                    if j == *prod_pos {
                        print!(". ");
                    }
                    print!("{} ", self.grammar.gvars[*id].name);
                }
                print!("\n")
            }

            print!("* * * * *\n");

            for (lookahead, map) in &self.parse_table[i] {
                for (tok_id, action) in map {
                    print!("  ({}) {} = ", lookahead, self.grammar.gvars[*tok_id].name);
                    match action {
                        ParseAction::Shift(next_state) => print!("Shift & Goto {}", next_state),
                        ParseAction::Reduce(gvar_id, prod_id) => {
                            print!("Reduce {} -> ", self.grammar.gvars[*gvar_id].name);
                            for id in &self.grammar.gvars[*gvar_id].productions[*prod_id] {
                                print!("{} ", self.grammar.gvars[*id].name);
                            }
                        },
                        ParseAction::ShiftReduce(gvar_id, prod_id) => {
                            print!("Shift & Reduce {} -> ", self.grammar.gvars[*gvar_id].name);
                            for id in &self.grammar.gvars[*gvar_id].productions[*prod_id] {
                                print!("{} ", self.grammar.gvars[*id].name);
                            }
                        }
                    }
                    print!("\n");
                }
            }

            print!("\n");
        }
    }
}

impl Parser for ParserLR {
    fn parse(&self, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>> {
        let mut nodes: Vec<Node> = Vec::new();

        // Stack of past states seen by the DFA
        let mut states: Vec<usize> = Vec::new();
        // Stack of nodes to be used in the next Reduction
        let mut node_stack: Vec<NodeId> = Vec::new();
        states.push(root);
        let mut pos = 0;

        // push the first token (transformed into the associated Terminal Gvar)
        let token = tokens.first().with_context(|| "Input sequence is empty.")?;
        nodes.push(self.new_node(0, self.grammar.token_gvar_map[&token.token_type], None));

        loop {
            // the last pushed element in nodes is also always the next input
            let next_node_id = nodes.len() - 1;
            let next_gvar_id = nodes[next_node_id].gvar_id;
            if next_gvar_id == 0 { break; }

            let cur_state_id = states.last().unwrap();
            let (_i, map) = &self.parse_table[*cur_state_id][0];

            match map.get(&next_gvar_id).with_context(|| format!("Couldn't find {}", self.grammar.gvars[next_gvar_id].name))?
            {
                ParseAction::Shift(next_state) => {
                    node_stack.push(next_node_id);
                    states.push(*next_state);
                    pos += 1;
                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, self.grammar.token_gvar_map[&tokens[pos].token_type], None));
                },
                ParseAction::Reduce(gvar_id, prod_id) => {
                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, *gvar_id, None));

                    let pop_count = self.grammar.gvars[*gvar_id].productions[*prod_id].len();
                    for _ in 0..pop_count {
                        states.pop();
                        let node_id = node_stack.pop().unwrap();
                        nodes[node_id].parent = Some(next_node_id);
                        nodes[next_node_id].children.push(node_id);
                    }
                    nodes[next_node_id].children.reverse();
                },
                ParseAction::ShiftReduce(gvar_id, prod_id) => {
                    let child_node_id = next_node_id;

                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, *gvar_id, None));

                    nodes[child_node_id].parent = Some(next_node_id);
                    nodes[next_node_id].children.push(child_node_id);

                    let pop_count = self.grammar.gvars[*gvar_id].productions[*prod_id].len() - 1;
                    for _ in 0..pop_count {
                        states.pop();
                        let node_id = node_stack.pop().unwrap();
                        nodes[node_id].parent = Some(next_node_id);
                        nodes[next_node_id].children.push(node_id);
                    }
                    nodes[next_node_id].children.reverse();
                },
            }
        }

        Ok(nodes)
    }

    fn get_required_lookahead(&self) -> usize {
        self.lookahead
    }
}

#[cfg(test)]
mod tests {
    use crate::grammar::*;
    use crate::tokenizer::*;

    use super::*;

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
}
