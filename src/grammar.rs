use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::mem::swap;

use anyhow::{Result, anyhow};

use crate::tokenizer::{TokenTypeId};

pub type ElementId = usize;
pub type ProductionId = usize;

#[derive(Clone)]
pub enum ElementType {
    Terminal(TokenTypeId),
    NonTerminal,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ProductionItem {
    pub elem_id: ElementId,
    pub kleene_closure: bool,
}

impl ProductionItem {
    pub fn new(elem_id: ElementId, kleene_closure: bool) -> Self {
        Self {
            elem_id,
            kleene_closure,
        }
    }
}

pub type Production = Vec<ProductionItem>;

#[derive(Clone)]
pub struct Element {
    pub id: ElementId,
    pub elem_type: ElementType,
    pub name: String,
    pub generated: bool,

    pub productions: Vec<Production>,
    pub follow_set: FollowSet,
    pub first_set: HashSet<ElementId>,
}

#[derive(Clone)]
pub struct Grammar {
    pub elems: Vec<Element>,
    pub elem_id_map: HashMap<String, ElementId>,
    pub token_elem_map: HashMap<TokenTypeId, ElementId>,

    ll_flag: bool,
    lr_flag: bool,
}

impl Grammar {
    pub fn new(ll_flag: bool, lr_flag: bool) -> Self {
        Self {
            elems: Vec::new(),
            elem_id_map: HashMap::new(),
            token_elem_map: HashMap::new(),
            ll_flag: ll_flag,
            lr_flag: lr_flag,
        }
    }
}

pub struct GrammarGenerator {
    pub elems: Vec<Element>,
    pub elem_map: HashMap<String, ElementId>,
    pub token_elem_map: HashMap<TokenTypeId, ElementId>,
    
    ll_flag: bool,
    lr_flag: bool,
}

/// Set of possible Symbol Sequences after a Element.
/// Each item is composed of a ProductionItems sequence and then a ElementId whose FollowSet would
/// then continue the sequence (possibly indefinitely)
type FollowSet = Vec<(Vec<ProductionItem>, ElementId)>;

fn has_follow(follow_set: &FollowSet, list1: &[ProductionItem], id1: ElementId) -> bool {
    for (list2, id2) in follow_set {
        if list1.len() == list2.len()
            && list1.iter().enumerate().all(|(i, itm)| 
                itm.elem_id == list2[i].elem_id && itm.kleene_closure == list2[i].kleene_closure)
            && id1.eq(id2)
        {
            return true;
        }
    }
    return false;
}

fn add_follow(follow_set: &mut FollowSet, list: &[ProductionItem], id: ElementId) {
    if !has_follow(follow_set, list, id) {
        follow_set.push((Vec::from(list), id));
    }
}

impl GrammarGenerator {
    pub fn new() -> Self {
        Self {
            elems: Vec::new(),
            elem_map: HashMap::new(),
            token_elem_map: HashMap::new(),
            ll_flag: false,
            lr_flag: false,
        }
    }

    pub fn copy_grammar(&mut self, grammar: &Grammar) {
        self.elems = grammar.elems.clone();
        self.elem_map = grammar.elem_id_map.clone();
        self.token_elem_map = grammar.token_elem_map.clone();
        self.ll_flag = grammar.ll_flag;
        self.lr_flag = grammar.lr_flag;
    }

    /// Generate Grammar directly from the current configuration.
    /// GrammarGenerator will need to be reconfigured after calling this method.
    pub fn generate(&mut self) -> Grammar {
        Self::get_follow_sets(&mut self.elems);
        Self::get_first_sets(&mut self.elems);

        let mut gram = Grammar {
            elems: Vec::new(),
            elem_id_map: HashMap::new(),
            token_elem_map: HashMap::new(),

            ll_flag: self.ll_flag,
            lr_flag: self.lr_flag,
        };

        swap(&mut self.elems, &mut gram.elems);
        swap(&mut self.elem_map, &mut gram.elem_id_map);
        swap(&mut self.token_elem_map, &mut gram.token_elem_map);

        gram
    }

    /// Generate an LL Grammar recognizing the same language as generate()
    /// Any Left-recursion is eliminated
    pub fn generate_ll(&mut self) -> Grammar {
        let mut gram = Grammar {
            elems: Vec::new(),
            elem_id_map: HashMap::new(),
            token_elem_map: HashMap::new(),
            
            ll_flag: true,
            lr_flag: false,
        };
        
        swap(&mut self.elems, &mut gram.elems);
        swap(&mut self.elem_map, &mut gram.elem_id_map);
        swap(&mut self.token_elem_map, &mut gram.token_elem_map);

        // TODO: error handling
        Self::eliminate_left_recursion(&mut gram.elems).unwrap();
        Self::left_factor(&mut gram.elems);
        Self::get_follow_sets(&mut gram.elems);
        Self::get_first_sets(&mut gram.elems);

        gram
    }

    fn eliminate_left_recursion(elems: &mut Vec<Element>) -> Result<()> {
        let mut new_elems = Vec::new();
        let mut leftrecursive = HashSet::new();

        for i in 0..elems.len() {
            for (j, prod) in elems[i].productions.iter().enumerate() {
                if !prod.is_empty() && prod[0].elem_id == elems[i].id {
                    leftrecursive.insert(j);
                }
            }

            if !leftrecursive.is_empty() {
                if leftrecursive.len() == elems[i].productions.len() {
                    return Err(anyhow!("Grammar is missing non-left-recursive production"));
                }

                let tail_elem_id = elems.len() + new_elems.len();
                let mut tail_elem = Element {
                    id: tail_elem_id,
                    elem_type: ElementType::NonTerminal,
                    generated: true,
                    name: elems[i].name.clone() + "'",
                    productions: vec![vec![]],
                    follow_set: Vec::new(),
                    first_set: HashSet::new(),
                };

                // create new productions for original and tail Elements
                let mut new_prods = Vec::new();
                for (i, prod) in elems[i].productions.iter().enumerate() {
                    if leftrecursive.contains(&i) {
                        // add to tail Element
                        let mut new_prod = prod[1..].to_vec();
                        new_prod.push(ProductionItem::new(tail_elem_id, false));
                        tail_elem.productions.push(new_prod);
                    }
                    else {
                        // add to original Element
                        let mut new_prod = prod.clone();
                        new_prod.push(ProductionItem::new(tail_elem_id, false));
                        new_prods.push(new_prod);
                    }
                }

                new_elems.push(tail_elem);
                swap(&mut new_prods, &mut elems[i].productions);
            }

            leftrecursive.clear();
        }

        elems.append(&mut new_elems);

        Ok(())
    }

    fn left_factor(elems: &mut Vec<Element>) {
        let mut new_elems: Vec<Element> = Vec::new();

        for i in 0..elems.len() {
            let mut map = HashMap::new();
            for (i, prod) in elems[i].productions.iter().enumerate() {
                if prod.is_empty() { continue; }
                if !map.contains_key(&prod[0].elem_id) {
                    map.insert(prod[0].elem_id, vec![]);
                }
                map.get_mut(&prod[0].elem_id).unwrap().push(i);
            }

            if map.iter().all(|(k,v)| v.len() == 1 || *k == elems[i].id ) { continue; }

            let tail_elem_id = elems.len() + new_elems.len();
            let mut tail_elem = Element {
                id: tail_elem_id,
                elem_type: ElementType::NonTerminal,
                generated: true,
                name: elems[i].name.clone() + "''",
                productions: vec![],
                follow_set: Vec::new(),
                first_set: HashSet::new(),
            };

            let mut new_prods = Vec::new();
            for (k, vec) in map {
                if vec.len() == 1 || k == elems[i].id {
                    for prod_id in vec {
                        new_prods.push(elems[i].productions[prod_id].clone());
                    }
                }
                else {
                    // find length of common prefix
                    let mut pos = 1;
                    while vec[1..].iter().all(|j| {
                        pos < elems[i].productions[*j].len()
                        && pos < elems[i].productions[vec[0]].len()
                        && elems[i].productions[*j][pos].elem_id == elems[i].productions[vec[0]][pos].elem_id
                    }) {
                        pos += 1;
                    }

                    // add unified prod with common prefix
                    let mut new_prod = elems[i].productions[vec[0]][0..pos].to_vec();
                    new_prod.push(ProductionItem::new(tail_elem_id, false));
                    new_prods.push(new_prod);

                    // add tail prods
                    for j in vec {
                        tail_elem.productions.push(elems[i].productions[j][pos..].to_vec());
                    }
                }
            }
            // replace original Element prods
            swap(&mut elems[i].productions, &mut new_prods);
            
            new_elems.push(tail_elem);
        }

        elems.append(&mut new_elems);
    }

    fn get_first_sets(elems: &mut Vec<Element>) {

        // add initial content
        for i in 0..elems.len() {
            for j in 0..elems[i].productions.len() {
                if elems[i].productions[j].len() > 0 {
                    let id = elems[i].productions[j][0].elem_id;
                    elems[i].first_set.insert(id);
                }
            }
        }

        loop {
            let mut modified = false;

            for i in 0..elems.len() {
                let mut app = HashSet::new();
                for j in elems[i].first_set.iter() {
                    for prod in &elems[*j].productions {
                        if prod.len() > 0 {
                            app.insert(prod[0].elem_id);
                        }
                    }
                }

                for j in app {
                    if !elems[i].first_set.contains(&j) {
                        elems[i].first_set.insert(j);
                        modified = true;
                    }
                }
            }

            if !modified { break; }
        }
    }

    fn get_follow_sets(elems: &mut Vec<Element>) {

        // add initial content of follow sets
        for i in 0..elems.len() {
            let mut new_follow_set = FollowSet::new();
            for j in 0..elems.len() {
                for rhs in &elems[j].productions {
                    for rhs_subid in 0..rhs.len() {
                        if rhs[rhs_subid].elem_id == elems[i].id {
                            add_follow(&mut new_follow_set, &rhs[(rhs_subid+1)..], elems[j].id);
                        }
                    }
                }
            }
            swap(&mut elems[i].follow_set, &mut new_follow_set);
        }

        // replace empty follow list in follow sets until none remain
        loop {
            let mut modified = false;

            for i in 0.. elems.len() {
                let mut new_follow_set = FollowSet::new();
                for (follow_list, id_after) in &elems[i].follow_set {
                    if follow_list.len() == 0 {
                        for (list2, id_after2) in &elems[*id_after].follow_set {
                            if !has_follow(&elems[i].follow_set, &list2, *id_after2) {
                                add_follow(&mut new_follow_set, &list2, *id_after2);
                                modified = true;
                            }
                        }
                    }
                    else {
                        add_follow(&mut new_follow_set, follow_list, *id_after);
                    }
                }

                swap(&mut elems[i].follow_set, &mut new_follow_set);
            }

            if !modified { break; }
        }
    }
   
    pub fn new_elem(&mut self, name: &str, elem_type: ElementType, generated: bool) -> ElementId {
        if self.elem_map.contains_key(name) {
            return self.elem_map[name];
        }
        
        let new_elem_id = self.elems.len();
        match elem_type {
            ElementType::Terminal(token_type) => {
                self.token_elem_map.insert(token_type, new_elem_id);
            },
            _ => (),
        }

        self.elems.push(Element {
            id: new_elem_id,
            elem_type: elem_type,
            generated: generated,
            name: String::from(name),
            
            productions: Vec::new(),
            follow_set: FollowSet::new(),
            first_set: HashSet::new(),
        });

        self.elem_map.insert(String::from(name), new_elem_id);
        
        new_elem_id
    }
    
    pub fn new_nonterm(&mut self, name: &str) -> ElementId {
        self.new_elem(name, ElementType::NonTerminal, false)
    }
   
    pub fn new_term(&mut self, name: &str, token_type: TokenTypeId) -> ElementId {
        self.new_elem(name, ElementType::Terminal(token_type), false)
    }

    pub fn new_prod(&mut self, def_id: ElementId, rhs: Production) -> ProductionId {
        let new_prod_id = self.elems[def_id].productions.len();

        if rhs.len() > 0 {
            // Left-recursion
            if rhs[0].elem_id == def_id {
                self.lr_flag = true;
            }
            
            // Right-recursion
            if rhs[rhs.len()-1].elem_id == def_id {
                self.ll_flag = true;
            }
        }

        self.elems[def_id].productions.push(rhs);

        new_prod_id
    }

    pub fn make_eps(&mut self, lhs_str: &str) -> ProductionId {
        self.make_prod(lhs_str, vec![])
    }

    pub fn make_prod(&mut self, lhs_str: &str, rhs_str: Vec<&str>) -> ProductionId {
        let def_id = self.elem_map[lhs_str];
        let mut rhs = Vec::new();
        for s in rhs_str {
            rhs.push(ProductionItem::new(self.elem_map[s], false));
        }
        self.new_prod(def_id, rhs)
    }

    pub fn make_prod2(&mut self, lhs_str: &str, rhs_str: Vec<(&str, bool)>) -> ProductionId {
        let def_id = self.elem_map[lhs_str];
        let mut rhs = Vec::new();
        for (s, kleene) in rhs_str {
            rhs.push(ProductionItem::new(self.elem_map[s], kleene));
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
pub fn show_follow_sets(elems: &Vec<Element>) {
    for elem in elems {
        println!("Follow sets for {}:", elem.name);
        for (list, id) in &elem.follow_set {
            for l in list {
                print!("{} ", elems[l.elem_id].name);
            }
            println!("[{}]", elems[*id].name);
        }
        println!("");
    }
}

#[allow(dead_code)]
pub fn show_prod_maps(elems: &Vec<Element>, prod_maps: &Vec<Vec<(usize, HashMap<TokenTypeId, ProductionId>)>>) {
    for elem in elems {
        println!("Prod Maps for {}:", elem.name);
        for (lookahead, prod_map) in &prod_maps[elem.id] {
            println!("({}): ", lookahead);
            for (id, prod_id) in prod_map {
                print!("\t{} = ", id);
                for itm in &elem.productions[*prod_id] {
                    print!("{} ", elems[itm.elem_id].name);
                }
                println!("")
            }
        }
    }
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.elems.iter().try_for_each(|elem|
            elem.productions.iter().try_for_each(|prod| {
                write!(f, "{} --> ", elem.name)
                .and(prod.iter().try_for_each(|child|
                    write!(f, "{} ", self.elems[child.elem_id].name)))
                .and(write!(f, "\n"))
            })
        )
        .and(self.elems.iter().try_for_each(|elem|
            match elem.elem_type {
                ElementType::Terminal(token_type_id) => {
                    writeln!(f, "{} --> {}", elem.name, token_type_id)
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
        // show_follow_sets(&gram.elems);
        // show_prod_maps(&gram.elems);

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
        // show_follow_sets(&gram.elems);
        // show_prod_maps(&gram.elems);

        // let mut vec: Vec<Token>;
        // vec = vec![Token {text: String::from("4"), token_type: tok_term}];
        // assert_eq!(gram.find_next(expr, &vec).unwrap(), prod3);
        // vec = vec![Token {text: String::from("+"), token_type: tok_op}];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), prod4);
        // vec = vec![tok_eof];
        // assert_eq!(gram.find_next(expr_tail, &vec).unwrap(), expr_tail_eps);
    }
}