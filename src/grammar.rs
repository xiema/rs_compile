use std::{collections::{HashMap}, fmt::Display, mem::swap};
use crate::tokenizer::{Token, TokenTypeId};

pub type GvarId = usize;
pub type ProductionId = usize;

type ProdMapQ = (ProductionId, Vec<GvarId>, GvarId, Vec<GvarId>);
pub type ProdMapType = (usize, HashMap<TokenTypeId, ProductionId>, Option<ProductionId>);

pub enum GvarType {
    Terminal,
    NonTerminal,
}

pub struct FollowSet {
    lists: Vec<(Vec<GvarId>, GvarId)>,
}

impl FollowSet {
    fn new() -> Self {
        Self {
            lists: Vec::new(),
        }
    }

    fn has_list(&self, list1: &[GvarId], id1: GvarId) -> bool {
        for (list2, id2) in &self.lists {
            if list1.eq(list2) && id1.eq(id2) {
                return true;
            }
        }
        return false;
    }

    fn add_list(&mut self, list: &[GvarId], id: GvarId) {
        if !self.has_list(list, id) {
            self.lists.push((Vec::from(list), id));
        }
    }
}

pub struct Gvar {
    pub id: GvarId,
    pub gvar_type: GvarType,
    pub name: String,
    token_type: Option<TokenTypeId>,
    eps: bool,

    pub productions: Vec<Vec<GvarId>>,
    pub prod_map: Vec<ProdMapType>,
    pub follow_set: FollowSet,
}

#[derive(PartialEq, Debug)]
pub enum GrammarClass {
    Undefined,
    LL(usize),
    LR(usize),
    Mixed(usize),
}

pub struct Grammar {
    pub gvars: Vec<Gvar>,

    pub class: GrammarClass,
    pub gvar_id_map: HashMap<String, GvarId>,
}

pub struct GrammarGenerator {
    gvars: Vec<Gvar>,
    gvar_id_map: HashMap<String, GvarId>,
}

impl GrammarGenerator {
    pub fn new() -> Self {
        Self {
            gvars: Vec::new(),
            gvar_id_map: HashMap::new(),
        }
    }

    /// Generate Grammar from the current configuration.
    /// GrammarGenerator will need to be reconfigured after calling this method.
    pub fn generate(&mut self) -> Grammar {
        self.get_follow_sets();
        // show_follow_sets(&self.gvars);

        // for i in 0..self.gvars.len() {
        //     self.gvars[i].follow_set = self.get_follow_set(i);
        // }
        
        for i in 0..self.gvars.len() {
            self.gvars[i].prod_map = self.get_prod_map(i);
        }

        let mut gram = Grammar {
            gvars: Vec::new(),
            class: self.get_grammar_class(),
            gvar_id_map: HashMap::new(),
        };

        swap(&mut self.gvars, &mut gram.gvars);
        swap(&mut self.gvar_id_map, &mut gram.gvar_id_map);

        gram
    }

    fn get_follow_sets(&mut self) {

        for i in 0..self.gvars.len() {
            let mut new_follow_set = FollowSet::new();
            for j in 0..self.gvars.len() {
                for rhs in &self.gvars[j].productions {
                    for rhs_subid in 0..rhs.len() {
                        if rhs[rhs_subid] == self.gvars[i].id {
                            new_follow_set.add_list(&rhs[(rhs_subid+1)..], self.gvars[j].id);
                            break;
                        }
                    }
                }
            }
            swap(&mut self.gvars[i].follow_set, &mut new_follow_set);
        }

        loop {
            let mut ok = true;

            for i in 0.. self.gvars.len() {
                let mut new_follow_set = FollowSet::new();
                for (list, id_after) in &self.gvars[i].follow_set.lists {
                    match list.len() {
                        0 => {
                            for (list2, id_after2) in &self.gvars[*id_after].follow_set.lists {
                                if !self.gvars[i].follow_set.has_list(&list2, *id_after2) {
                                    new_follow_set.add_list(&list2, *id_after2);
                                    ok = false;
                                }
                            }
                        },
                        _ => {
                            new_follow_set.add_list(list, *id_after);
                        }
                    }
                }

                swap(&mut self.gvars[i].follow_set, &mut new_follow_set);
            }

            if ok { break; }
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
                        new_follow_set.add_list(&rhs[(rhs_subid+1)..], upper_gvar.id);
                        break;
                    }
                }
            }
        }

        new_follow_set
    }

    fn push_if(v: &mut Vec<ProdMapQ>, obj: ProdMapQ) {
        if !v.contains(&obj) {
            v.push(obj);
        }
    }

    fn get_prod_map(&self, gvar_id: GvarId) -> Vec<ProdMapType> {
        let mut prod_map: Vec<ProdMapType> = Vec::new();

        if self.gvars[gvar_id].productions.len() == 1 {
            return prod_map;
        }
        
        let mut q1: Vec<(ProductionId, Vec<GvarId>, GvarId, Vec<GvarId>)> = self.gvars[gvar_id].productions.iter().enumerate().map(|(i, p)| (i, p.clone(), gvar_id, Vec::new())).collect();
        let mut q2: Vec<(ProductionId, Vec<GvarId>, GvarId, Vec<GvarId>)> = Vec::new();
        let mut lookahead = 1;

        while !q1.is_empty() {
            // replace nonterminals
            loop {
                let mut ok = true;
                for (prod_id, prod, id_after, seen_ids) in &mut q1 {
                    match prod.len() {
                        0 => {
                            // if !seen_ids.contains(id_after) {
                            seen_ids.push(*id_after);
                            for (list, new_id_after) in &self.gvars[*id_after].follow_set.lists {
                                Self::push_if(&mut q2, (*prod_id, list.clone(), *new_id_after, seen_ids.clone()));
                                ok = false;
                            }
                            // }
                        },
                        _ => {
                            match self.gvars[prod[0]].gvar_type {
                                GvarType::NonTerminal => {
                                    for sub_prod in &self.gvars[prod[0]].productions {
                                        let mut new_prod = sub_prod.clone();
                                        new_prod.extend_from_slice(&prod[1..]);
                                        Self::push_if(&mut q2, (*prod_id, new_prod, *id_after, seen_ids.clone()));
                                    }
                                    ok = false;
                                },
                                GvarType::Terminal => {
                                    Self::push_if(&mut q2, (*prod_id, prod.clone(), *id_after, seen_ids.clone()));
                                }
                            }
                        },
                    }
                }

                swap(&mut q1, &mut q2);
                q2.clear();
                if ok { break; }
            }

            let mut count: HashMap<GvarId, Vec<ProductionId>> = HashMap::new();
            for (prod_id, prod, _, _) in &q1 {
                if !count.contains_key(&prod[0]) {
                    count.insert(prod[0], Vec::new());
                }
                if !count[&prod[0]].contains(prod_id) {
                    count.get_mut(&prod[0]).unwrap().push(*prod_id);
                }
            }

            let mut cur_map: HashMap<TokenTypeId, ProductionId> = HashMap::new();
            for (gvar_id, prod_ids) in &count {
                if prod_ids.len() > 1 { continue; }
                let prod_id = prod_ids[0];
                let token_id = self.gvars[*gvar_id].token_type.unwrap();
                cur_map.insert(token_id, prod_id);
                q1.retain(|(id, p, _, _)|
                    *id != prod_id
                    || self.gvars[p[0]].token_type.is_none()
                    || self.gvars[p[0]].token_type.unwrap() != token_id
                );
            }
            
            if q1.len() > 0 && q1.iter().all(|(prod_id, _, _, _)| *prod_id == q1[0].0) {
                prod_map.push((lookahead, cur_map, Some(q1[0].0)));
                q1.clear();
            }
            else if cur_map.len() > 0 {
                prod_map.push((lookahead, cur_map, None));
            }

            for (prod_id, prod, id_after, seen_ids) in &q1 {
                q2.push((*prod_id, Vec::from(&prod[1..]), *id_after, seen_ids.clone()));
            }
            
            swap(&mut q1, &mut q2);
            q2.clear();
            lookahead += 1;
        }

        prod_map
    }

    fn get_grammar_class(&self) -> GrammarClass {
        let mut gclass = GrammarClass::Undefined;
        let mut n = 0;

        for gvar in &self.gvars {
            for (lookahead, _, _) in &gvar.prod_map {
                n = std::cmp::max(*lookahead, n);
            }
        }

        for gvar in &self.gvars {
            for rhs in &gvar.productions {
                // eps
                if rhs.len() == 0 { continue; }

                // Left-recursion
                if rhs[0] == gvar.id {
                    gclass = match gclass {
                        GrammarClass::Undefined => GrammarClass::LR(n),
                        GrammarClass::LL(_) => GrammarClass::Mixed(n),
                        GrammarClass::LR(_) => GrammarClass::LR(n),
                        GrammarClass::Mixed(_) => GrammarClass::Mixed(n),
                    }
                }
                
                // Right-recursion
                if rhs[rhs.len()-1] == gvar.id {
                    gclass = match gclass {
                        GrammarClass::Undefined => GrammarClass::LL(n),
                        GrammarClass::LL(_) => GrammarClass::LL(n),
                        GrammarClass::LR(_) => GrammarClass::Mixed(n),
                        GrammarClass::Mixed(_) => GrammarClass::Mixed(n),
                    }
                }
            }
        }

        gclass
    }
    
    pub fn new_nonterm(&mut self, name: &str) -> GvarId {
        let new_gvar_id = self.gvars.len();

        self.gvars.push(Gvar {
            id: new_gvar_id,
            gvar_type: GvarType::NonTerminal,
            name: String::from(name),
            token_type: None,
            eps: false,

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
            eps: false,

            productions: Vec::new(),
            prod_map: Vec::new(),
            follow_set: FollowSet::new(),
        });

        self.gvar_id_map.insert(String::from(name), new_gvar_id);

        new_gvar_id
    }

    pub fn new_prod(&mut self, def_id: GvarId, rhs: Vec<GvarId>) -> ProductionId {
        let new_prod_id = self.gvars[def_id].productions.len();

        self.gvars[def_id].productions.push(rhs);

        new_prod_id
    }

    pub fn make_eps(&mut self, def_id: GvarId) -> ProductionId {
        // self.gvars[def_id].productions.push(vec![]);
        self.gvars[def_id].eps = true;
        self.new_prod(def_id, vec![])
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
        match self.class {
            GrammarClass::Undefined => false,
            GrammarClass::LL(_) => true,
            GrammarClass::LR(_) => false,
            GrammarClass::Mixed(_) => true,
        }
    }

    pub fn is_lr(&self) -> bool {
        match self.class {
            GrammarClass::Undefined => false,
            GrammarClass::LL(_) => false,
            GrammarClass::LR(_) => true,
            GrammarClass::Mixed(_) => true,
        }
    }

    pub fn find_next(&self, gvar: GvarId, tokens: &[Token]) -> Result<Option<ProductionId>, &str> {
        if self.gvars[gvar].productions.len() == 1 {
            return Ok(Some(0));
        }

        if tokens.len() == 0 { panic!("Tokens length is 0!"); }
        for i in 0..tokens.len() {
            let token = &tokens[i];
            for (lookahead, map, else_id) in &self.gvars[gvar].prod_map {
                if *lookahead > i + 1 { break; }
                if *lookahead == i + 1 && map.contains_key(&token.token_type) {
                    return Ok(Some(map[&token.token_type]));
                }
                if else_id.is_some() {
                    return Ok(*else_id);
                }
            }
        }
        if self.gvars[gvar].eps {
            return Ok(None);
        }
        return Err("Couldn't find production");
    }
}

#[allow(dead_code)]
pub fn show_follow_sets(gvars: &Vec<Gvar>) {
    for gvar in gvars {
        println!("Follow sets for {}:", gvar.name);
        for (list, id) in &gvar.follow_set.lists {
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
        let expr_list_tail_eps = gram_gen.make_eps(expr_list_tail);
        let prod3 = gram_gen.make_prod("Expression", vec!["Term", "Expression_Tail"]);
        let prod4 = gram_gen.make_prod("Expression_Tail", vec!["Operator", "Expression"]);
        let expr_tail_eps = gram_gen.make_eps(expr_tail);

        let gram = gram_gen.generate();

        // println!("{}", gram);
        // show_follow_sets(&gram.gvars);
        // show_prod_maps(&gram.gvars);

        let mut vec: Vec<Token>;
        vec = vec![Token {text: String::from("4"), token_type: tok_term}];
        assert_eq!(gram.find_next(expr, &vec).unwrap().unwrap(), prod3);
        vec = vec![Token {text: String::from("+"), token_type: tok_op}];
        assert_eq!(gram.find_next(expr_tail, &vec).unwrap().unwrap(), prod4);
        vec = vec![tok_eof];
        assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), Some(expr_tail_eps));
    }
}