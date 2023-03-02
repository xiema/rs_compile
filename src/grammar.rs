use regex::Regex;
use std::{collections::{HashSet, HashMap}, fmt::Display, mem::swap};
use crate::tokenizer::{Token, TokenTypeId};

pub type GvarId = usize;
pub type ProductionId = usize;


#[derive(Clone, Copy, PartialEq)]
pub enum GvarType {
    Terminal,
    NonTerminal,
}

pub struct Gvar {
    pub id: GvarId,
    pub gvar_type: GvarType,
    pub name: String,
    token_type: Option<TokenTypeId>,
    eps: bool,

    pub productions: Vec<Vec<GvarId>>,
    pub first: HashSet<Regex>,
    pub follow: HashSet<Regex>,
}

#[derive(PartialEq)]
pub enum GrammarClass {
    Undefined,
    LL(i32),
    LR(i32),
    Mixed(i32),
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
        let mut gram = Grammar {
            gvars: Vec::new(),
            class: self.get_grammar_class(),
            gvar_id_map: HashMap::new(),
        };

        swap(&mut self.gvars, &mut gram.gvars);
        swap(&mut self.gvar_id_map, &mut gram.gvar_id_map);

        gram
    }

    fn get_grammar_class(&self) -> GrammarClass {
        let mut gclass = GrammarClass::Undefined;
        for gvar in &self.gvars {
            for rhs in &gvar.productions {
                // Left-recursion flag
                if rhs[0] == gvar.id {
                    let n = 1;
                    gclass = match gclass {
                        GrammarClass::Undefined => GrammarClass::LR(n),
                        GrammarClass::LL(m) => GrammarClass::Mixed(n),
                        GrammarClass::LR(m) => GrammarClass::LR(n),
                        GrammarClass::Mixed(m) => GrammarClass::Mixed(n),
                    }
                }
                
                // Right-recursion flag
                if rhs[rhs.len()-1] == gvar.id {
                    let n = 1;
                    gclass = match gclass {
                        GrammarClass::Undefined => GrammarClass::LL(n),
                        GrammarClass::LL(m) => GrammarClass::LL(n),
                        GrammarClass::LR(m) => GrammarClass::Mixed(n),
                        GrammarClass::Mixed(m) => GrammarClass::Mixed(n),
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
            first: HashSet::new(),
            follow: HashSet::new(),
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
            first: HashSet::new(),
            follow: HashSet::new(),
        });

        self.gvar_id_map.insert(String::from(name), new_gvar_id);

        new_gvar_id
    }

    pub fn new_prod(&mut self, def_id: GvarId, rhs: Vec<GvarId>) -> ProductionId {
        let new_prod_id = self.gvars[def_id].productions.len();

        self.gvars[def_id].productions.push(rhs);

        new_prod_id
    }

    pub fn make_eps(&mut self, def_id: GvarId) {
        self.gvars[def_id].eps = true;
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
    pub fn is_ll(&self) -> bool {
        match self.class {
            GrammarClass::Undefined => false,
            GrammarClass::LL(n) => true,
            GrammarClass::LR(n) => false,
            GrammarClass::Mixed(n) => true,
        }
    }

    pub fn is_lr(&self) -> bool {
        match self.class {
            GrammarClass::Undefined => false,
            GrammarClass::LL(n) => false,
            GrammarClass::LR(n) => true,
            GrammarClass::Mixed(n) => true,
        }
    }

    fn has_first(&self, gvar: GvarId, token: &Token) -> bool {
        match self.gvars[gvar].gvar_type {
            GvarType::Terminal => self.gvars[gvar].token_type.unwrap().eq(&token.token_type),
            GvarType::NonTerminal => self.gvars[gvar].productions.iter().any(|p| self.has_first(p[0], token))
        }
    }

    pub fn find_next(&self, gvar: GvarId, token: Option<&Token>) -> Result<Option<ProductionId>, &str> {
        match token {
            None => (),
            Some(t) => {
                for i in 0..self.gvars[gvar].productions.len() {
                    if self.has_first(self.gvars[gvar].productions[i][0], t) {
                        return Ok(Some(i));
                    }
                }
            }
        }
        if self.gvars[gvar].eps {
            return Ok(None);
        }
        return Err("Couldn't find production");
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
        for gvar in self.gvars.iter().filter(|n| n.gvar_type == GvarType::Terminal) {
            println!("{} --> {}", gvar.name, gvar.token_type.unwrap());
        }
        Ok(())
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn grammar_test() {
        let mut gram_gen = GrammarGenerator::new();

        let tok_term: TokenTypeId = 0;
        let tok_op: TokenTypeId = 1;
        
        let expr = gram_gen.new_nonterm("Expression");
        let expr_tail = gram_gen.new_nonterm("Expression_Tail");
        let term = gram_gen.new_term("Term", tok_term);
        let op = gram_gen.new_term("Operator", tok_op);

        let prod1 = gram_gen.new_prod(expr, vec![term, expr_tail]);
        let prod2 = gram_gen.new_prod(expr_tail, vec![op, expr]);
        gram_gen.make_eps(expr_tail);

        let gram = gram_gen.generate();

        println!("{}", gram);


        let tok = Token {text: String::from("4"), token_type: tok_term};
        assert_eq!(gram.find_next(expr, Some(&tok)).unwrap().unwrap(), prod1);
        let tok = Token {text: String::from("+"), token_type: tok_op};
        assert_eq!(gram.find_next(expr_tail, Some(&tok)).unwrap().unwrap(), prod2);
        assert_eq!(gram.find_next(expr_tail, None).unwrap(), None);
    }
}