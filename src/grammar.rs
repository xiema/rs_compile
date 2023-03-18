use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::mem::swap;

use anyhow::{Result, anyhow};

use crate::tokenizer::{TokenTypeId};

pub type GvarId = usize;
pub type ProductionId = usize;

#[derive(Clone)]
pub enum GvarType {
    Terminal(TokenTypeId),
    NonTerminal,
}

#[derive(Clone)]
pub struct Gvar {
    pub id: GvarId,
    pub gvar_type: GvarType,
    pub name: String,

    pub productions: Vec<Vec<GvarId>>,
    pub follow_set: FollowSet,
    pub first_set: HashSet<GvarId>,
}

#[derive(Clone)]
pub struct Grammar {
    pub gvars: Vec<Gvar>,
    pub gvar_id_map: HashMap<String, GvarId>,
    pub token_gvar_map: HashMap<TokenTypeId, GvarId>,

    ll_flag: bool,
    lr_flag: bool,
}

pub struct GrammarGenerator {
    gvars: Vec<Gvar>,
    gvar_id_map: HashMap<String, GvarId>,
    
    ll_flag: bool,
    lr_flag: bool,
    token_gvar_map: HashMap<TokenTypeId, GvarId>,
}

/// Set of possible Symbol Sequences after a Gvar.
/// Each item is composed of a symbol sequence and then a GvarId whose FollowSet would
/// then continue the sequence (possibly indefinitely)
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

impl GrammarGenerator {
    pub fn new() -> Self {
        Self {
            gvars: Vec::new(),
            gvar_id_map: HashMap::new(),
            token_gvar_map: HashMap::new(),
            ll_flag: false,
            lr_flag: false,
        }
    }

    /// Generate Grammar directly from the current configuration.
    /// GrammarGenerator will need to be reconfigured after calling this method.
    pub fn generate(&mut self) -> Grammar {
        Self::get_follow_sets(&mut self.gvars);
        Self::get_first_sets(&mut self.gvars);

        let mut gram = Grammar {
            gvars: Vec::new(),
            gvar_id_map: HashMap::new(),
            token_gvar_map: HashMap::new(),

            ll_flag: self.ll_flag,
            lr_flag: self.lr_flag,
        };

        swap(&mut self.gvars, &mut gram.gvars);
        swap(&mut self.gvar_id_map, &mut gram.gvar_id_map);
        swap(&mut self.token_gvar_map, &mut gram.token_gvar_map);

        gram
    }

    /// Generate an LL Grammar recognizing the same language as generate()
    /// Any Left-recursion is eliminated
    pub fn generate_ll(&mut self) -> Grammar {
        let mut gram = Grammar {
            gvars: Vec::new(),
            gvar_id_map: HashMap::new(),
            token_gvar_map: HashMap::new(),
            
            ll_flag: true,
            lr_flag: false,
        };
        
        swap(&mut self.gvars, &mut gram.gvars);
        swap(&mut self.gvar_id_map, &mut gram.gvar_id_map);
        swap(&mut self.token_gvar_map, &mut gram.token_gvar_map);

        // TODO: error handling
        Self::eliminate_left_recursion(&mut gram.gvars).unwrap();
        Self::left_factor(&mut gram.gvars);
        Self::get_follow_sets(&mut gram.gvars);
        Self::get_first_sets(&mut gram.gvars);

        gram
    }

    fn eliminate_left_recursion(gvars: &mut Vec<Gvar>) -> Result<()> {
        let mut new_gvars = Vec::new();
        let mut leftrecursive = HashSet::new();

        for i in 0..gvars.len() {
            for (j, prod) in gvars[i].productions.iter().enumerate() {
                if !prod.is_empty() && prod[0] == gvars[i].id {
                    leftrecursive.insert(j);
                }
            }

            if !leftrecursive.is_empty() {
                if leftrecursive.len() == gvars[i].productions.len() {
                    return Err(anyhow!("Grammar is missing non-left-recursive production"));
                }

                let tail_gvar_id = gvars.len() + new_gvars.len();
                let mut tail_gvar = Gvar {
                    id: tail_gvar_id,
                    gvar_type: GvarType::NonTerminal,
                    name: gvars[i].name.clone() + "'",
                    productions: vec![vec![]],
                    follow_set: Vec::new(),
                    first_set: HashSet::new(),
                };

                // create new productions for original and tail gvars
                let mut new_prods = Vec::new();
                for (i, prod) in gvars[i].productions.iter().enumerate() {
                    if leftrecursive.contains(&i) {
                        // add to tail gvar
                        let mut new_prod = prod[1..].to_vec();
                        new_prod.push(tail_gvar_id);
                       tail_gvar.productions.push(new_prod);
                    }
                    else {
                        // add to original gvar
                        let mut new_prod = prod.clone();
                        new_prod.push(tail_gvar_id);
                        new_prods.push(new_prod);
                    }
                }

                new_gvars.push(tail_gvar);
                swap(&mut new_prods, &mut gvars[i].productions);
            }

            leftrecursive.clear();
        }

        gvars.append(&mut new_gvars);

        Ok(())
    }

    fn left_factor(gvars: &mut Vec<Gvar>) {
        let mut new_gvars: Vec<Gvar> = Vec::new();

        for i in 0..gvars.len() {
            let mut map = HashMap::new();
            for (i, prod) in gvars[i].productions.iter().enumerate() {
                if prod.is_empty() { continue; }
                if !map.contains_key(&prod[0]) {
                    map.insert(prod[0], vec![]);
                }
                map.get_mut(&prod[0]).unwrap().push(i);
            }

            if map.iter().all(|(k,v)| v.len() == 1 || *k == gvars[i].id ) { continue; }

            let tail_gvar_id = gvars.len() + new_gvars.len();
            let mut tail_gvar = Gvar {
                id: tail_gvar_id,
                gvar_type: GvarType::NonTerminal,
                name: gvars[i].name.clone() + "''",
                productions: vec![],
                follow_set: Vec::new(),
                first_set: HashSet::new(),
            };

            let mut new_prods = Vec::new();
            for (k, vec) in map {
                if vec.len() == 1 || k == gvars[i].id {
                    for prod_id in vec {
                        new_prods.push(gvars[i].productions[prod_id].clone());
                    }
                }
                else {
                    // find length of common prefix
                    let mut pos = 1;
                    while vec[1..].iter().all(|j| {
                        pos < gvars[i].productions[*j].len()
                        && pos < gvars[i].productions[vec[0]].len()
                        && gvars[i].productions[*j][pos] == gvars[i].productions[vec[0]][pos]
                    }) {
                        pos += 1;
                    }

                    // add unified prod with common prefix
                    let mut new_prod = gvars[i].productions[vec[0]][0..pos].to_vec();
                    new_prod.push(tail_gvar_id);
                    new_prods.push(new_prod);

                    // add tail prods
                    for j in vec {
                        tail_gvar.productions.push(gvars[i].productions[j][pos..].to_vec());
                    }
                }
            }
            // replace original gvar prods
            swap(&mut gvars[i].productions, &mut new_prods);
            
            new_gvars.push(tail_gvar);
        }

        gvars.append(&mut new_gvars);
    }

    fn get_first_sets(gvars: &mut Vec<Gvar>) {

        // add initial content
        for i in 0..gvars.len() {
            for j in 0..gvars[i].productions.len() {
                if gvars[i].productions[j].len() > 0 {
                    let id = gvars[i].productions[j][0];
                    gvars[i].first_set.insert(id);
                }
            }
        }

        loop {
            let mut modified = false;

            for i in 0..gvars.len() {
                let mut app = HashSet::new();
                for j in gvars[i].first_set.iter() {
                    for prod in &gvars[*j].productions {
                        if prod.len() > 0 {
                            app.insert(prod[0]);
                        }
                    }
                }

                for j in app.iter() {
                    if !gvars[i].first_set.contains(j) {
                        gvars[i].first_set.insert(*j);
                        modified = true;
                    }
                }
            }

            if !modified { break; }
        }
    }

    fn get_follow_sets(gvars: &mut Vec<Gvar>) {

        // add initial content of follow sets
        for i in 0..gvars.len() {
            let mut new_follow_set = FollowSet::new();
            for j in 0..gvars.len() {
                for rhs in &gvars[j].productions {
                    for rhs_subid in 0..rhs.len() {
                        if rhs[rhs_subid] == gvars[i].id {
                            add_follow(&mut new_follow_set, &rhs[(rhs_subid+1)..], gvars[j].id);
                        }
                    }
                }
            }
            swap(&mut gvars[i].follow_set, &mut new_follow_set);
        }

        // replace empty follow list in follow sets until none remain
        loop {
            let mut modified = false;

            for i in 0.. gvars.len() {
                let mut new_follow_set = FollowSet::new();
                for (follow_list, id_after) in &gvars[i].follow_set {
                    if follow_list.len() == 0 {
                        for (list2, id_after2) in &gvars[*id_after].follow_set {
                            if !has_follow(&gvars[i].follow_set, &list2, *id_after2) {
                                add_follow(&mut new_follow_set, &list2, *id_after2);
                                modified = true;
                            }
                        }
                    }
                    else {
                        add_follow(&mut new_follow_set, follow_list, *id_after);
                    }
                }

                swap(&mut gvars[i].follow_set, &mut new_follow_set);
            }

            if !modified { break; }
        }
    }
   
    pub fn new_nonterm(&mut self, name: &str) -> GvarId {
        if self.gvar_id_map.contains_key(name) {
            return self.gvar_id_map[name];
        }

        let new_gvar_id = self.gvars.len();

        self.gvars.push(Gvar {
            id: new_gvar_id,
            gvar_type: GvarType::NonTerminal,
            name: String::from(name),

            productions: Vec::new(),
            follow_set: FollowSet::new(),
            first_set: HashSet::new(),
        });

        self.gvar_id_map.insert(String::from(name), new_gvar_id);
        
        new_gvar_id
    }

    pub fn new_term(&mut self, name: &str, token_type: TokenTypeId) -> GvarId {
        if self.gvar_id_map.contains_key(name) {
            return self.gvar_id_map[name];
        }

        let new_gvar_id = self.gvars.len();

        self.gvars.push(Gvar {
            id: new_gvar_id,
            gvar_type: GvarType::Terminal(token_type),
            name: String::from(name),

            productions: Vec::new(),
            follow_set: FollowSet::new(),
            first_set: HashSet::new(),
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
                self.lr_flag = true;
            }
            
            // Right-recursion
            if rhs[rhs.len()-1] == def_id {
                self.ll_flag = true;
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
    pub fn is_parseable_ll(&self) -> bool {
        !self.lr_flag
    }

    pub fn is_parseable_lr(&self) -> bool {
        true
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.gvars.iter().try_for_each(|gvar|
            gvar.productions.iter().try_for_each(|prod| {
                write!(f, "{} --> ", gvar.name)
                .and(prod.iter().try_for_each(|child|
                    write!(f, "{} ", self.gvars[*child].name)))
                .and(write!(f, "\n"))
            })
        )
        .and(self.gvars.iter().try_for_each(|gvar|
            match gvar.gvar_type {
                GvarType::Terminal(token_type_id) => {
                    writeln!(f, "{} --> {}", gvar.name, token_type_id)
                },
                _ => Ok(())
            }
        ))
    }
}



#[allow(unused_variables)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::*;

    #[test]
    fn grammar_test() {
        let mut gram_gen = GrammarGenerator::new();

        let tok_term: TokenTypeId = 0;
        let tok_op: TokenTypeId = 1;
        let tok_eof = Token::new("", -1, 0, 0, 0);
        
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

        println!("{}", gram);
        // show_follow_sets(&gram.gvars);
        // show_prod_maps(&gram.gvars);

        // let mut vec: Vec<Token>;
        // vec = vec![Token {text: String::from("4"), token_type: tok_term}];
        // assert_eq!(gram.find_next(expr, &vec).unwrap(), prod3);
        // vec = vec![Token {text: String::from("+"), token_type: tok_op}];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), prod4);
        // vec = vec![tok_eof];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), expr_tail_eps);
    }

    #[test]
    fn grammar_convert_ll_test() {
        let mut gram_gen = GrammarGenerator::new();

        let tok_term: TokenTypeId = 0;
        let tok_op: TokenTypeId = 1;
        let tok_eof = Token::new("", -1, 0, 0, 0);
        
        let prog = gram_gen.new_nonterm("Program");
        let expr_list = gram_gen.new_nonterm("Expression_List");
        let expr = gram_gen.new_nonterm("Expression");
        let term = gram_gen.new_term("Term", tok_term);
        let op = gram_gen.new_term("Operator", tok_op);
        let eof = gram_gen.new_term("EOF", tok_eof.token_type);

        let prod0 = gram_gen.make_prod("Program", vec!["Expression_List", "EOF"]);
        let prod1 = gram_gen.make_prod("Expression_List", vec!["Expression_List", "Expression"]);
        let prod1 = gram_gen.make_prod("Expression_List", vec!["Expression"]);
        let prod3 = gram_gen.make_prod("Expression", vec!["Expression", "Operator", "Term"]);
        let prod3 = gram_gen.make_prod("Expression", vec!["Term"]);

        let gram = gram_gen.generate_ll();

        // println!("{}", gram);
        // show_follow_sets(&gram.gvars);
        // show_prod_maps(&gram.gvars);

        // let mut vec: Vec<Token>;
        // vec = vec![Token {text: String::from("4"), token_type: tok_term}];
        // assert_eq!(gram.find_next(expr, &vec).unwrap(), prod3);
        // vec = vec![Token {text: String::from("+"), token_type: tok_op}];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), prod4);
        // vec = vec![tok_eof];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), expr_tail_eps);
    }
}