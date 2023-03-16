use std::collections::{VecDeque, HashMap, HashSet};
use std::mem::swap;

use crate::grammar::{GvarId, ProductionId, Grammar, GvarType, ParseAction};
use crate::tokenizer::{Token, TokenTypeId};

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

pub struct ParserLL {
    grammar: Grammar,
    parse_table: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>,
    lookahead: usize,
}

impl ParserLL {
    pub fn new(grammar: &Grammar) -> Self {
        if !grammar.is_parseable_ll() {
            panic!("ParserLL can't parse grammar");
        }

        let table = Self::get_parse_table(grammar);
        let n = table.iter().fold(0, 
                |acc, x| 
                    std::cmp::max(acc, x.iter().fold(0,
                        |acc, x| 
                            std::cmp::max(acc, x.0))));

        Self {
            grammar: grammar.clone(),
            parse_table: table,
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

    fn get_parse_table(grammar: &Grammar) -> Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>  {
        let mut table: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>> = Vec::new();

        for gvar_id in 0..grammar.gvars.len() {
            // println!("Finding ProdMap for {}", &self.gvars[gvar_id].name);

            let mut prod_map = Vec::new();

            if grammar.gvars[gvar_id].productions.len() == 1 {
                table.push(prod_map);
                continue;
            }
            
            // Tuple of (Production Id, RHS Gvars List, Parent Node Gvar)
            // RHS Gvars (converted to corresponding Tokens) and Production Ids are inspected to find if a unique token 
            //   can be found at a particular lookahead position.
            // Parent Node Gvar is used to extend the RHS Gvar Vector if the lookahead exceeds the current length
            let mut s1: HashSet<(ProductionId, Vec<GvarId>, GvarId)> = grammar.gvars[gvar_id].productions.iter().enumerate().map(|(i, p)| (i, p.clone(), gvar_id)).collect();
            let mut s2: HashSet<(ProductionId, Vec<GvarId>, GvarId)> = HashSet::new();
            let mut lookahead = 1;

            while !s1.is_empty() {
                
                // ensure at least one terminal at front of every rhs
                loop {
                    let mut modified = false;

                    for (prod_id, rhs, id_after) in &s1 {
                        if rhs.len() == 0 {
                            // replace empty rhs with follow set
                            for (list, new_id_after) in &grammar.gvars[*id_after].follow_set {
                                s2.insert((*prod_id, list.clone(), *new_id_after));
                                modified = true;
                            }
                        }
                        else {
                            match grammar.gvars[rhs[0]].gvar_type {
                                GvarType::NonTerminal => {
                                    // replace nonterminals at front of rhs with corresponding productions
                                    for sub_prod in &grammar.gvars[rhs[0]].productions {
                                        let mut new_prod = sub_prod.clone();
                                        new_prod.extend_from_slice(&rhs[1..]);
                                        s2.insert((*prod_id, new_prod, *id_after));
                                    }
                                    modified = true;
                                },
                                GvarType::Terminal => {
                                    // retain terminals
                                    s2.insert((*prod_id, rhs.clone(), *id_after));
                                }
                            }
                        }
                    }

                    swap(&mut s1, &mut s2);
                    s2.clear();
                    if !modified { break; }
                }


                // map of terminals to the possible productions if the terminal is seen at the current position
                let mut terminal_to_prod: HashMap<GvarId, Vec<ProductionId>> = HashMap::new();
                for (prod_id, rhs, _) in &s1 {
                    if !terminal_to_prod.contains_key(&rhs[0]) {
                        terminal_to_prod.insert(rhs[0], Vec::new());
                    }
                    if !terminal_to_prod[&rhs[0]].contains(prod_id) {
                        terminal_to_prod.get_mut(&rhs[0]).unwrap().push(*prod_id);
                    }
                }

                // possible prod map at this lookahead position
                let mut new_map: HashMap<TokenTypeId, ProductionId> = HashMap::new();
                for (gvar_id, prod_ids) in &terminal_to_prod {
                    // skip if more than 1 production possible with this token at this position
                    if prod_ids.len() > 1 { continue; }

                    // add this token-production pair to prod_map at this lookahead position
                    let prod_id = prod_ids[0];
                    let token_id = grammar.gvars[*gvar_id].token_type.unwrap();
                    new_map.insert(token_id, prod_id);

                    // remove all similar productions (same lookahead token and production id, but possibly different production trees/routes)
                    s1.retain(|(id, p, _)|
                        *id != prod_id
                        || grammar.gvars[p[0]].token_type.is_none()
                        || grammar.gvars[p[0]].token_type.unwrap() != token_id
                    );
                }
                
                if !new_map.is_empty() {
                    // add the prod map if something was inserted
                    prod_map.push((lookahead, new_map));
                }

                // advance all rhs lists by 1 token
                for (prod_id, rhs, id_after) in &s1 {
                    s2.insert((*prod_id, Vec::from(&rhs[1..]), *id_after));
                }
                
                swap(&mut s1, &mut s2);
                s2.clear();
                lookahead += 1;
            }

            table.push(prod_map);
        }

        table
    }

    fn find_next(&self, gvar: GvarId, tokens: &[Token]) -> Result<ProductionId, &str> {
        if self.grammar.gvars[gvar].productions.len() == 1 {
            return Ok(0);
        }

        if tokens.len() == 0 { panic!("Tokens length is 0!"); }
        for i in 0..tokens.len() {
            let token = &tokens[i];
            for (lookahead, map) in &self.parse_table[gvar] {
                if *lookahead > i + 1 { break; }
                if *lookahead == i + 1 && map.contains_key(&token.token_type) {
                    return Ok(map[&token.token_type]);
                }
            }
        }
        return Err("Couldn't find production");
    }
}

impl Parser for ParserLL {
    fn parse(&self, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>, &str> {

        let mut nodes: Vec<Node> = Vec::new();
        let mut stk: VecDeque<NodeId> = VecDeque::new();
        let mut pos = 0;
        nodes.push(self.new_node(0, root, None));
        stk.push_front(0);

        while !stk.is_empty() {
            let cur_node_id = stk.pop_front().unwrap();
            match self.grammar.gvars[nodes[cur_node_id].gvar_id].gvar_type {
                GvarType::Terminal => {
                    nodes[cur_node_id].token = Some(tokens[pos].clone());
                    pos += 1;
                },
                GvarType::NonTerminal => {
                    let prod_id = self.find_next(nodes[cur_node_id].gvar_id, &tokens[pos..])
                        .unwrap_or_else(|err| panic!("Parser error: {}, {}", err, tokens[pos].text));
            
                    // store production produced by this nonterm
                    nodes[cur_node_id].prod_id = Some(prod_id);
                    let prod_len = self.grammar.gvars[nodes[cur_node_id].gvar_id].productions[prod_id].len();

                    for i in 0..prod_len {
                        let child_gvar_id = self.grammar.gvars[nodes[cur_node_id].gvar_id].productions[prod_id][i];
                        let new_node_id = nodes.len();
                        // create new node
                        nodes.push(self.new_node(new_node_id, child_gvar_id, Some(cur_node_id)));
                        // associate new node as child of parent node
                        nodes[cur_node_id].children.push(new_node_id);
                    }
                    // push new nodes onto stack
                    for child_id in nodes[cur_node_id].children.iter().rev() {
                        stk.push_front(*child_id);
                    }
                }
            }
        }

        return Ok(nodes);
    }

    fn get_required_lookahead(&self) -> usize {
        self.lookahead
    }
}

struct ParserLR {
    grammar: Grammar,
    parse_table: Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>>,
    lookahead: usize,
}

impl ParserLR {
    pub fn new(grammar: &Grammar) -> Self {
        let table = Self::get_parse_table(grammar);
        let n = table.iter().fold(0, 
                |acc, x| 
                    std::cmp::max(acc, x.iter().fold(0,
                        |acc, x| 
                            std::cmp::max(acc, x.0))));

        Self {
            grammar: grammar.clone(),
            parse_table: table,
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

    fn get_parse_table(grammar: &Grammar) -> Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>> {
        // add initial state

        let mut table: Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>> = Vec::new();
        let mut state_defs: Vec<HashSet<(GvarId, ProductionId, usize)>> = Vec::new();

        let mut cur_state_id = 0;
        
        // starting state
        let mut state_def = HashSet::new();
        state_def.insert((0, 0, 0));
        state_defs.push(state_def);

        while cur_state_id < state_defs.len() {
            let mut closures = HashSet::new();
            let mut follow_ids = HashSet::new();
            let mut action_map: HashMap<GvarId, ParseAction> = HashMap::new();

            // get closures and follow ids
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
                    // shift-reduce a possible production if it is the only one compatible with the follow_id
                    for (gvar_id, prod_id, _) in &new_state {
                        action_map.insert(follow_id, ParseAction::ShiftReduce(*gvar_id, *prod_id));
                    }
                }
                else {
                    // shift

                    // add the new state if it is unique, or else get the existing state_id
                    let new_state_id = match state_defs.iter().enumerate()
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
                    action_map.insert(follow_id, ParseAction::Shift(new_state_id));
                }
            }

            table.push(vec![(1, action_map)]);

            cur_state_id += 1;
        }

        table
    }
}

impl Parser for ParserLR {
    fn parse(&self, tokens: &Vec<Token>, root: GvarId) -> Result<Vec<Node>, &str> {
        let mut nodes: Vec<Node> = Vec::new();

        let mut states: Vec<usize> = Vec::new();
        let mut node_stack: Vec<NodeId> = Vec::new();
        states.push(0);
        let mut pos = 0;

        nodes.push(self.new_node(0, self.grammar.token_gvar_map[&tokens[pos].token_type], None));

        loop {
            let next_node_id = nodes.len() - 1;
            if nodes[next_node_id].gvar_id == 0 { break; }

            let state_id = states.last().unwrap();
            let (i, map) = &self.parse_table[*state_id][0];

            match map[&nodes[next_node_id].gvar_id] {
                ParseAction::Shift(next_state) => {
                    node_stack.push(next_node_id);
                    states.push(next_state);
                    pos += 1;
                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, self.grammar.token_gvar_map[&tokens[pos].token_type], None));
                },
                ParseAction::Reduce(gvar_id, prod_id) => {
                    let next_node_id = nodes.len();
                    nodes.push(self.new_node(next_node_id, gvar_id, None));

                    let pop_count = self.grammar.gvars[gvar_id].productions[prod_id].len();
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
                    nodes.push(self.new_node(next_node_id, gvar_id, None));

                    nodes[child_node_id].parent = Some(next_node_id);
                    nodes[next_node_id].children.push(child_node_id);

                    let pop_count = self.grammar.gvars[gvar_id].productions[prod_id].len() - 1;
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
            TokenPattern::Single("[[:space:]]")
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
        
        let mut parser = ParserLL::new(&gram);
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
            TokenPattern::Single("[[:space:]]")
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
        
        let mut parser = ParserLR::new(&gram);
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
            TokenPattern::Single("[[:space:]]")
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
        
        let mut parser = ParserLL::new(&gram);
        let nodes = parser.parse(&tokens, 0).unwrap();

        // display_ast(0, &nodes, &gram, 0);
    }
}
