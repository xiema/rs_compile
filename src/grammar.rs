use std::{collections::{HashSet, HashMap}, fmt::Display, mem::swap};
use crate::tokenizer::{Token, TokenTypeId};

pub type GvarId = usize;
pub type ProductionId = usize;

pub type ProdMap = Vec<(usize, HashMap<TokenTypeId, ProductionId>, Option<ProductionId>)>;

pub enum GvarType {
    Terminal,
    NonTerminal,
}

type FollowSet = Vec<(Vec<GvarId>, GvarId)>;

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
    token_type: Option<TokenTypeId>,

    pub productions: Vec<Vec<GvarId>>,
    pub prod_map: ProdMap,
    pub follow_set: FollowSet,
}

pub type GrammarClass = (bool, bool, i32);

pub struct Grammar {
    pub gvars: Vec<Gvar>,

    pub class: GrammarClass,
    pub gvar_id_map: HashMap<String, GvarId>,
}

pub struct GrammarGenerator {
    gvars: Vec<Gvar>,
    gvar_id_map: HashMap<String, GvarId>,

    class: GrammarClass,
}

impl GrammarGenerator {
    pub fn new() -> Self {
        Self {
            gvars: Vec::new(),
            gvar_id_map: HashMap::new(),
            class: (false, false, -1),
        }
    }

    /// Generate Grammar from the current configuration.
    /// GrammarGenerator will need to be reconfigured after calling this method.
    pub fn generate(&mut self) -> Grammar {
        self.get_follow_sets();
        show_follow_sets(&self.gvars);

        // for i in 0..self.gvars.len() {
        //     self.gvars[i].follow_set = self.get_follow_set(i);
        // }
        
        if !self.class.1 {
            for i in 0..self.gvars.len() {
                println!("Finding ProdMap for {}", &self.gvars[i].name);
                self.gvars[i].prod_map = self.get_prod_map(i);
            }
        }

        // get required lookahead        
        let n = self.gvars.iter().fold(0,
            |acc, x| std::cmp::max(acc, x.prod_map.iter().fold(0, 
            |acc, x| std::cmp::max(acc, x.0))));
        self.class = (self.class.0, self.class.1, n as i32);

        let mut gram = Grammar {
            gvars: Vec::new(),
            class: self.class,
            gvar_id_map: HashMap::new(),
        };

        swap(&mut self.gvars, &mut gram.gvars);
        swap(&mut self.gvar_id_map, &mut gram.gvar_id_map);

        gram
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

    fn get_prod_map(&self, gvar_id: GvarId) -> ProdMap {
        let mut prod_map: ProdMap = Vec::new();

        if self.gvars[gvar_id].productions.len() == 1 {
            return prod_map;
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
            
            let last = s1.iter().last();
            if s1.len() > 0 && s1.iter().all(|(prod_id, _, _)| *prod_id == last.unwrap().0) {
                // if all remaining productions are the same, add an "else" clause
                prod_map.push((lookahead, new_map, Some(last.unwrap().0)));
                s1.clear();
            }
            else if !new_map.is_empty() {
                // add the prod map if something was inserted
                prod_map.push((lookahead, new_map, None));
            }

            // advance all rhs lists by 1 token
            for (prod_id, rhs, id_after) in &s1 {
                s2.insert((*prod_id, Vec::from(&rhs[1..]), *id_after));
            }
            
            swap(&mut s1, &mut s2);
            s2.clear();
            lookahead += 1;
        }

        prod_map
    }
    
    pub fn new_nonterm(&mut self, name: &str) -> GvarId {
        let new_gvar_id = self.gvars.len();

        self.gvars.push(Gvar {
            id: new_gvar_id,
            gvar_type: GvarType::NonTerminal,
            name: String::from(name),
            token_type: None,

            productions: Vec::new(),
            prod_map: Vec::new(),
            follow_set: FollowSet::new(),
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
            prod_map: Vec::new(),
            follow_set: FollowSet::new(),
        });

        self.gvar_id_map.insert(String::from(name), new_gvar_id);

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
            for (lookahead, map, else_id) in &self.gvars[gvar].prod_map {
                if *lookahead > i + 1 { break; }
                if *lookahead == i + 1 && map.contains_key(&token.token_type) {
                    return Ok(map[&token.token_type]);
                }
                if else_id.is_some() {
                    return Ok(else_id.unwrap());
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
pub fn show_prod_maps(gvars: &Vec<Gvar>) {
    for gvar in gvars {
        println!("Prod Maps for {}:", gvar.name);
        for (lookahead, prod_map, else_id) in &gvar.prod_map {
            println!("({}): ", lookahead);
            for (id, prod_id) in prod_map {
                print!("\t{} = ", id);
                for id2 in &gvar.productions[*prod_id] {
                    print!("{} ", gvars[*id2].name);
                }
                println!("")
            }
            match else_id {
                None => (),
                Some(id) => {
                    print!("\telse = ");
                    for id in &gvar.productions[*id] {
                        print!("{} ", gvars[*id].name);
                    }
                    println!("");
                }
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