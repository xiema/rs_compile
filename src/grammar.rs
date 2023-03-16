use std::{collections::{HashMap}, fmt::Display, mem::swap};
use crate::tokenizer::{TokenTypeId};

pub type GvarId = usize;
pub type ProductionId = usize;

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Gvar {
    pub id: GvarId,
    pub gvar_type: GvarType,
    pub name: String,
    pub token_type: Option<TokenTypeId>,

    pub productions: Vec<Vec<GvarId>>,
    pub follow_set: FollowSet,
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

    /// Generate Grammar from the current configuration.
    /// GrammarGenerator will need to be reconfigured after calling this method.
    pub fn generate(&mut self) -> Grammar {
        self.get_follow_sets();

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
   
    pub fn new_nonterm(&mut self, name: &str) -> GvarId {
        let new_gvar_id = self.gvars.len();

        self.gvars.push(Gvar {
            id: new_gvar_id,
            gvar_type: GvarType::NonTerminal,
            name: String::from(name),
            token_type: None,

            productions: Vec::new(),
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
            follow_set: FollowSet::new(),
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
    use crate::tokenizer::*;

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

        // let mut vec: Vec<Token>;
        // vec = vec![Token {text: String::from("4"), token_type: tok_term}];
        // assert_eq!(gram.find_next(expr, &vec).unwrap(), prod3);
        // vec = vec![Token {text: String::from("+"), token_type: tok_op}];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), prod4);
        // vec = vec![tok_eof];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), expr_tail_eps);
    }
}