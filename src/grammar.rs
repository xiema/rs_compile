use std::{collections::{HashSet, HashMap}, fmt::Display, mem::swap};
use crate::tokenizer::{Token, TokenTypeId};

pub type GvarId = usize;
pub type ProductionId = usize;

pub enum GvarType {
    Terminal,
    NonTerminal,
}

type FollowSet = Vec<(Vec<GvarId>, GvarId)>;

pub enum ParseAction {
    Shift(usize),
    Reduce(GvarId, ProductionId),
    ShiftReduce(GvarId, ProductionId),
}

fn has_follow(follow_set: &FollowSet, list1: &[GvarId], id1: GvarId) -> bool {
    for (list2, id2) in follow_set {
        if list1.eq(list2) && id1.eq(id2) {
            return true;
        }
    }
    return false;
}

fn add_follow(follow_set: &mut FollowSet, list: &[GvarId], id: GvarId) {
    if !has_follow(follow_set, list, id) {
        follow_set.push((Vec::from(list), id));
    }
}

pub struct Gvar {
    pub id: GvarId,
    pub gvar_type: GvarType,
    pub name: String,
    pub token_type: Option<TokenTypeId>,

    pub productions: Vec<Vec<GvarId>>,
    pub follow_set: FollowSet,
    pub follow_tokens: HashSet<GvarId>,
    pub first_set: HashSet<GvarId>,
}

pub type GrammarClass = (bool, bool, i32);

pub struct Grammar {
    pub gvars: Vec<Gvar>,
    pub class: GrammarClass,
    pub gvar_id_map: HashMap<String, GvarId>,
    pub token_gvar_map: HashMap<TokenTypeId, GvarId>,
    pub parse_table_lr: Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>>,
    pub parse_table_ll: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>,
}

pub struct GrammarGenerator {
    gvars: Vec<Gvar>,
    gvar_id_map: HashMap<String, GvarId>,
    
    class: GrammarClass,
    token_gvar_map: HashMap<TokenTypeId, GvarId>,
}

impl GrammarGenerator {
    pub fn new() -> Self {
        Self {
            gvars: Vec::new(),
            gvar_id_map: HashMap::new(),
            token_gvar_map: HashMap::new(),
            class: (false, false, -1),
        }
    }

    /// Generate Grammar from the current configuration.
    /// GrammarGenerator will need to be reconfigured after calling this method.
    pub fn generate(&mut self) -> Grammar {
        self.get_follow_sets();
        self.get_first_sets();
        self.get_follow_tokens();
        
        // if !self.class.1 {
        //     self.get_prod_mapll();

        //     // get required lookahead        
            
        // }
        // else {
        //     self.get_prod_maplr();
        // }


        let mut gram = Grammar {
            gvars: Vec::new(),
            class: self.class,
            gvar_id_map: HashMap::new(),
            token_gvar_map: HashMap::new(),
            parse_table_lr: if !self.class.0 { self.get_prod_maplr() } else { Vec::new() } ,
            parse_table_ll: if !self.class.1 { self.get_prod_mapll() } else { Vec::new() } ,
        };

        let n = gram.parse_table_ll.iter().fold(0, 
            |acc, x| 
                std::cmp::max(acc, x.iter().fold(0,
                    |acc, x| 
                        std::cmp::max(acc, x.0))));
        gram.class = (self.class.0, self.class.1, n as i32);

        swap(&mut self.gvars, &mut gram.gvars);
        swap(&mut self.gvar_id_map, &mut gram.gvar_id_map);
        swap(&mut self.token_gvar_map, &mut gram.token_gvar_map);

        gram
    }

    fn get_follow_tokens(&mut self) {
        loop {
            let mut modified = false;

            for i in 0..self.gvars.len() {
                let mut follow_tokens = HashSet::new();

                for (fol, _) in &self.gvars[i].follow_set {
                    let j = fol[0];
                    match self.gvars[j].gvar_type {
                        GvarType::Terminal => {
                            if !self.gvars[i].follow_tokens.contains(&j) {
                                follow_tokens.insert(j);
                            }
                        },
                        GvarType::NonTerminal => {
                            for k in &self.gvars[j].follow_tokens {
                                if !self.gvars[i].follow_tokens.contains(k) {
                                    follow_tokens.insert(*k);
                                }
                            }
                        }
                    }
                }

                if follow_tokens.len() > 0 {
                    modified = true;
                    self.gvars[i].follow_tokens.extend(follow_tokens);
                }
            }

            if !modified { break; }
        }
    }

    fn get_first_sets(&mut self) {

        // add initial content
        for i in 0..self.gvars.len() {
            for j in 0..self.gvars[i].productions.len() {
                if self.gvars[i].productions[j].len() > 0 {
                    let id = self.gvars[i].productions[j][0];
                    self.gvars[i].first_set.insert(id);
                }
            }
        }

        loop {
            let mut modified = false;

            for i in 0..self.gvars.len() {
                let mut app = HashSet::new();
                for j in self.gvars[i].first_set.iter() {
                    for prod in &self.gvars[*j].productions {
                        if prod.len() > 0 {
                            app.insert(prod[0]);
                        }
                    }
                }

                for j in app.iter() {
                    if !self.gvars[i].first_set.contains(j) {
                        self.gvars[i].first_set.insert(*j);
                        modified = true;
                    }
                }
            }

            if !modified { break; }
        }
    }

    fn get_follow_sets(&mut self) {

        // add initial content of follow sets
        for i in 0..self.gvars.len() {
            let mut new_follow_set = FollowSet::new();
            for j in 0..self.gvars.len() {
                for rhs in &self.gvars[j].productions {
                    for rhs_subid in 0..rhs.len() {
                        if rhs[rhs_subid] == self.gvars[i].id {
                            add_follow(&mut new_follow_set, &rhs[(rhs_subid+1)..], self.gvars[j].id);
                            break;
                        }
                    }
                }
            }
            swap(&mut self.gvars[i].follow_set, &mut new_follow_set);
        }

        // replace empty follow list in follow sets until none remain
        loop {
            let mut modified = false;

            for i in 0.. self.gvars.len() {
                let mut new_follow_set = FollowSet::new();
                for (follow_list, id_after) in &self.gvars[i].follow_set {
                    if follow_list.len() == 0 {
                        for (list2, id_after2) in &self.gvars[*id_after].follow_set {
                            if !has_follow(&self.gvars[i].follow_set, &list2, *id_after2) {
                                add_follow(&mut new_follow_set, &list2, *id_after2);
                                modified = true;
                            }
                        }
                    }
                    else {
                        add_follow(&mut new_follow_set, follow_list, *id_after);
                    }
                }

                swap(&mut self.gvars[i].follow_set, &mut new_follow_set);
            }

            if !modified { break; }
        }
    }

    #[allow(dead_code)]
    #[deprecated]
    fn get_follow_set(&self, gvar_id: GvarId) -> FollowSet {
        let mut new_follow_set = FollowSet::new();
        for upper_gvar in &self.gvars {
            for rhs in &upper_gvar.productions {
                for rhs_subid in 0..rhs.len() {
                    if rhs[rhs_subid] == gvar_id {
                        add_follow(&mut new_follow_set, &rhs[(rhs_subid+1)..], upper_gvar.id);
                        break;
                    }
                }
            }
        }

        new_follow_set
    }

    fn get_prod_mapll(&self) -> Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>  {
        let mut table: Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>> = Vec::new();

        for gvar_id in 0..self.gvars.len() {
            println!("Finding ProdMap for {}", &self.gvars[gvar_id].name);

            let mut prod_map = Vec::new();

            if self.gvars[gvar_id].productions.len() == 1 {
                table.push(prod_map);
                continue;
            }
            
            // Tuple of (Production Id, RHS Gvars List, Parent Node Gvar)
            // RHS Gvars (converted to corresponding Tokens) and Production Ids are inspected to find if a unique token 
            //   can be found at a particular lookahead position.
            // Parent Node Gvar is used to extend the RHS Gvar Vector if the lookahead exceeds the current length
            let mut s1: HashSet<(ProductionId, Vec<GvarId>, GvarId)> = self.gvars[gvar_id].productions.iter().enumerate().map(|(i, p)| (i, p.clone(), gvar_id)).collect();
            let mut s2: HashSet<(ProductionId, Vec<GvarId>, GvarId)> = HashSet::new();
            let mut lookahead = 1;

            while !s1.is_empty() {
                
                // ensure at least one terminal at front of every rhs
                loop {
                    let mut modified = false;

                    for (prod_id, rhs, id_after) in &s1 {
                        if rhs.len() == 0 {
                            // replace empty rhs with follow set
                            for (list, new_id_after) in &self.gvars[*id_after].follow_set {
                                s2.insert((*prod_id, list.clone(), *new_id_after));
                                modified = true;
                            }
                        }
                        else {
                            match self.gvars[rhs[0]].gvar_type {
                                GvarType::NonTerminal => {
                                    // replace nonterminals at front of rhs with corresponding productions
                                    for sub_prod in &self.gvars[rhs[0]].productions {
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
                    let token_id = self.gvars[*gvar_id].token_type.unwrap();
                    new_map.insert(token_id, prod_id);

                    // remove all similar productions (same lookahead token and production id, but possibly different production trees/routes)
                    s1.retain(|(id, p, _)|
                        *id != prod_id
                        || self.gvars[p[0]].token_type.is_none()
                        || self.gvars[p[0]].token_type.unwrap() != token_id
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

    fn get_prod_maplr(&self) -> Vec<Vec<(usize, HashMap<GvarId, ParseAction>)>> {
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
                if *prod_pos < self.gvars[*gvar_id].productions[*prod_id].len() {
                    let id = self.gvars[*gvar_id].productions[*prod_id][*prod_pos];
                    if matches!(self.gvars[id].gvar_type, GvarType::NonTerminal) {
                        for i in 0..self.gvars[id].productions.len() {
                            closures.insert((id, i));
                        }
                    }
                    follow_ids.insert(id);
                }
            }
            loop {
                let mut new_closures = HashSet::new();
                for (gvar_id, prod_id) in &closures {
                    let id = self.gvars[*gvar_id].productions[*prod_id][0];
                    if matches!(self.gvars[id].gvar_type, GvarType::NonTerminal) {
                        for i in 0..self.gvars[id].productions.len() {
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
                if *prod_pos == self.gvars[*gvar_id].productions[*prod_id].len() {
                    for id in &self.gvars[*gvar_id].follow_tokens {
                        if follow_ids.contains(&id) || seen.contains(&id) {
                            panic!("Grammar is not LR(1)");
                        }
                        action_map.insert(*id, ParseAction::Reduce(*gvar_id, *prod_id));
                        seen.insert(id);
                    }
                }
            }

            // actions for each follow id
            for follow_id in follow_ids {
                let mut new_state = HashSet::new();
                let mut reduce = false;
                
                // bases
                for (gvar_id, prod_id, prod_pos) in &state_defs[cur_state_id] {
                    if *prod_pos < self.gvars[*gvar_id].productions[*prod_id].len() {
                        let id = self.gvars[*gvar_id].productions[*prod_id][*prod_pos];
                        if id == follow_id {
                            new_state.insert((*gvar_id, *prod_id, prod_pos + 1));
                            reduce |= prod_pos + 1 == self.gvars[*gvar_id].productions[*prod_id].len();
                        }
                    }
                }
                // closures
                for (gvar_id, prod_id) in &closures {
                    let id = self.gvars[*gvar_id].productions[*prod_id][0];
                    if id == follow_id {
                        new_state.insert((*gvar_id, *prod_id, 1));
                        reduce |= 1 == self.gvars[*gvar_id].productions[*prod_id].len();
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
    
    pub fn new_nonterm(&mut self, name: &str) -> GvarId {
        let new_gvar_id = self.gvars.len();

        self.gvars.push(Gvar {
            id: new_gvar_id,
            gvar_type: GvarType::NonTerminal,
            name: String::from(name),
            token_type: None,

            productions: Vec::new(),
            follow_set: FollowSet::new(),
            first_set: HashSet::new(),
            follow_tokens: HashSet::new(),
        });

        self.gvar_id_map.insert(String::from(name), new_gvar_id);
        
        new_gvar_id
    }

    pub fn new_term(&mut self, name: &str, token_type: TokenTypeId) -> GvarId {
        let new_gvar_id = self.gvars.len();

        self.gvars.push(Gvar {
            id: new_gvar_id,
            gvar_type: GvarType::Terminal,
            name: String::from(name),
            token_type: Some(token_type),

            productions: Vec::new(),
            follow_set: FollowSet::new(),
            first_set: HashSet::new(),
            follow_tokens: HashSet::new(),
        });

        self.gvar_id_map.insert(String::from(name), new_gvar_id);
        self.token_gvar_map.insert(token_type, new_gvar_id);

        new_gvar_id
    }

    pub fn new_prod(&mut self, def_id: GvarId, rhs: Vec<GvarId>) -> ProductionId {
        let new_prod_id = self.gvars[def_id].productions.len();

        if rhs.len() > 0 {
            // Left-recursion
            if rhs[0] == def_id {
                self.class = (self.class.0, true, self.class.2);
            }
            
            // Right-recursion
            if rhs[rhs.len()-1] == def_id {
                self.class = (true, self.class.1, self.class.2);
            }
        }

        self.gvars[def_id].productions.push(rhs);

        new_prod_id
    }

    pub fn make_eps(&mut self, lhs_str: &str) -> ProductionId {
        self.make_prod(lhs_str, vec![])
    }

    pub fn make_prod(&mut self, lhs_str: &str, rhs_str: Vec<&str>) -> ProductionId {
        let def_id = self.gvar_id_map[lhs_str];
        let mut rhs = Vec::new();
        for s in rhs_str {
            rhs.push(self.gvar_id_map[s]);
        }
        self.new_prod(def_id, rhs)
    }
}

impl Grammar {
    #[allow(dead_code)]
    pub fn is_ll(&self) -> bool {
        self.class.0
    }

    pub fn is_lr(&self) -> bool {
        self.class.1
    }

    pub fn find_next(&self, gvar: GvarId, tokens: &[Token]) -> Result<ProductionId, &str> {
        if self.gvars[gvar].productions.len() == 1 {
            return Ok(0);
        }

        if tokens.len() == 0 { panic!("Tokens length is 0!"); }
        for i in 0..tokens.len() {
            let token = &tokens[i];
            for (lookahead, map) in &self.parse_table_ll[gvar] {
                if *lookahead > i + 1 { break; }
                if *lookahead == i + 1 && map.contains_key(&token.token_type) {
                    return Ok(map[&token.token_type]);
                }
            }
        }
        return Err("Couldn't find production");
    }
}

#[allow(dead_code)]
pub fn show_follow_sets(gvars: &Vec<Gvar>) {
    for gvar in gvars {
        println!("Follow sets for {}:", gvar.name);
        for (list, id) in &gvar.follow_set {
            for l in list {
                print!("{} ", gvars[*l].name);
            }
            println!("[{}]", gvars[*id].name);
        }
        println!("");
    }
}

#[allow(dead_code)]
pub fn show_prod_maps(gvars: &Vec<Gvar>, prod_maps: &Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>) {
    for gvar in gvars {
        println!("Prod Maps for {}:", gvar.name);
        for (lookahead, prod_map) in &prod_maps[gvar.id] {
            println!("({}): ", lookahead);
            for (id, prod_id) in prod_map {
                print!("\t{} = ", id);
                for id2 in &gvar.productions[*prod_id] {
                    print!("{} ", gvars[*id2].name);
                }
                println!("")
            }
        }
    }
}

impl Display for Grammar {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for gvar in &self.gvars {
            for prod in &gvar.productions {
                print!("{} --> ", gvar.name);
                for child in prod {
                    print!("{} ", self.gvars[*child].name);
                }
                print!("\n");
            }
        }
        for gvar in &self.gvars {
            match gvar.gvar_type {
                GvarType::Terminal => {
                    println!("{} --> {}", gvar.name, gvar.token_type.unwrap());
                },
                _ => ()
            }
            
        }
        Ok(())
    }
}



#[allow(unused_variables)]
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn grammar_test() {
        let mut gram_gen = GrammarGenerator::new();

        let tok_term: TokenTypeId = 0;
        let tok_op: TokenTypeId = 1;
        let tok_eof = Token {text: String::from(""), token_type: -1};
        
        let prog = gram_gen.new_nonterm("Program");
        let expr_list = gram_gen.new_nonterm("Expression_List");
        let expr_list_tail = gram_gen.new_nonterm("Expression_List_Tail");
        let expr = gram_gen.new_nonterm("Expression");
        let expr_tail = gram_gen.new_nonterm("Expression_Tail");
        let term = gram_gen.new_term("Term", tok_term);
        let op = gram_gen.new_term("Operator", tok_op);
        let eof = gram_gen.new_term("EOF", tok_eof.token_type);

        let prod0 = gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        let prod1 = gram_gen.make_prod("Expression_List", vec!["Expression", "Expression_List_Tail"]);
        let prod2 = gram_gen.make_prod("Expression_List_Tail", vec!["Expression", "Expression_List_Tail"]);
        let expr_list_tail_eps = gram_gen.make_eps("Expression_List_Tail");
        let prod3 = gram_gen.make_prod("Expression", vec!["Term", "Expression_Tail"]);
        let prod4 = gram_gen.make_prod("Expression_Tail", vec!["Operator", "Expression"]);
        let expr_tail_eps = gram_gen.make_eps("Expression_Tail");

        let gram = gram_gen.generate();

        // println!("{}", gram);
        // show_follow_sets(&gram.gvars);
        // show_prod_maps(&gram.gvars);

        let mut vec: Vec<Token>;
        vec = vec![Token {text: String::from("4"), token_type: tok_term}];
        assert_eq!(gram.find_next(expr, &vec).unwrap(), prod3);
        vec = vec![Token {text: String::from("+"), token_type: tok_op}];
        assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), prod4);
        vec = vec![tok_eof];
        assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), expr_tail_eps);
    }
}