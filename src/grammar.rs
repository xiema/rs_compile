use regex::Regex;
use std::{collections::{HashSet, HashMap}, fmt::Display, mem::swap, slice::Iter};

pub type NodeDefId = usize;
pub type ProductionId = usize;


#[derive(Clone, Copy, PartialEq)]
pub enum NodeType {
    Terminal,
    NonTerminal,
}

pub struct NodeDef {
    pub id: NodeDefId,
    pub node_type: NodeType,
    pub name: String,
    pattern: Option<Regex>,
    eps: bool,

    pub productions: Vec<Vec<NodeDefId>>,
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
    pub nodes: Vec<NodeDef>,

    pub class: GrammarClass,
    pub node_id_map: HashMap<String, NodeDefId>,
}

pub struct GrammarGenerator {
    nodes: Vec<NodeDef>,

    class: GrammarClass,
    node_id_map: HashMap<String, NodeDefId>,
}

impl GrammarGenerator {
    pub fn new() -> Self {
        let gram = Self {
            nodes: Vec::new(),
            class: GrammarClass::Undefined,
            node_id_map: HashMap::new(),
        };
        gram
    }

    pub fn generate(&mut self) -> Grammar {
        let class = GrammarClass::Undefined;

        let mut gram = Grammar {
            nodes: Vec::new(),
            class: class,
            node_id_map: HashMap::new(),
        };

        swap(&mut self.nodes, &mut gram.nodes);
        swap(&mut self.node_id_map, &mut gram.node_id_map);
        swap(&mut self.class, &mut gram.class);

        gram
    }
    
    pub fn new_nonterm(&mut self, name: &str) -> NodeDefId {
        let new_node_id = self.nodes.len();

        self.nodes.push(NodeDef {
            id: new_node_id,
            node_type: NodeType::NonTerminal,
            name: String::from(name),
            pattern: None,
            eps: false,

            productions: Vec::new(),
            first: HashSet::new(),
            follow: HashSet::new(),
        });

        self.node_id_map.insert(String::from(name), new_node_id);
        
        new_node_id
    }

    pub fn new_term(&mut self, name: &str, pattern: &str) -> NodeDefId {
        let new_node_id = self.nodes.len();

        self.nodes.push(NodeDef {
            id: new_node_id,
            node_type: NodeType::Terminal,
            name: String::from(name),
            pattern: Some(Regex::new((String::from("^(") + pattern + ")").as_str()).unwrap()),
            eps: false,

            productions: Vec::new(),
            first: HashSet::new(),
            follow: HashSet::new(),
        });

        self.node_id_map.insert(String::from(name), new_node_id);

        new_node_id
    }

    pub fn new_prod(&mut self, def_id: NodeDefId, rhs: Vec<NodeDefId>) -> ProductionId {
        let new_prod_id = self.nodes[def_id].productions.len();

        // Left-recursion flag
        if rhs[0] == def_id {
            let n = 1;
            self.class = match self.class {
                GrammarClass::Undefined => GrammarClass::LR(n),
                GrammarClass::LL(m) => GrammarClass::Mixed(n),
                GrammarClass::LR(m) => GrammarClass::LR(n),
                GrammarClass::Mixed(m) => GrammarClass::Mixed(n),
            }
        }
        
        // Right-recursion flag
        if rhs[rhs.len()-1] == def_id {
            let n = 1;
            self.class = match self.class {
                GrammarClass::Undefined => GrammarClass::LL(n),
                GrammarClass::LL(m) => GrammarClass::LL(n),
                GrammarClass::LR(m) => GrammarClass::Mixed(n),
                GrammarClass::Mixed(m) => GrammarClass::Mixed(n),
            }
        }

        self.nodes[def_id].productions.push(rhs);

        new_prod_id
    }

    pub fn make_eps(&mut self, def_id: NodeDefId) {
        self.nodes[def_id].eps = true;
    }

    pub fn make_prod(&mut self, lhs_str: &str, rhs_str: Vec<&str>) -> ProductionId {
        let def_id = self.node_id_map[lhs_str];
        let mut rhs = Vec::new();
        for s in rhs_str {
            rhs.push(self.node_id_map[s]);
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

    fn has_first(&self, node_def: NodeDefId, tok: &str) -> bool {
        match self.nodes[node_def].node_type {
            NodeType::Terminal => self.nodes[node_def].pattern.as_ref().unwrap().is_match(tok),
            NodeType::NonTerminal => self.nodes[node_def].productions.iter().any(|p| self.has_first(p[0], tok))
        }
    }

    pub fn find_next(&self, node_def: NodeDefId, tok: Option<&str>) -> Result<Option<ProductionId>, &str> {
        match tok {
            None => (),
            Some(s) => {
                for i in 0..self.nodes[node_def].productions.len() {
                    if self.has_first(self.nodes[node_def].productions[i][0], s) {
                        return Ok(Some(i));
                    }
                }
            }
        }
        if self.nodes[node_def].eps {
            return Ok(None);
        }
        return Err("Couldn't find production");
    }
}

impl Display for Grammar {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in &self.nodes {
            for prod in &node.productions {
                print!("{} --> ", node.name);
                for child in prod {
                    print!("{} ", self.nodes[*child].name);
                }
                print!("\n");
            }
        }
        for node in self.nodes.iter().filter(|n| n.node_type == NodeType::Terminal) {
            println!("{} --> {}", node.name, node.pattern.as_ref().unwrap());
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
        
        let expr = gram_gen.new_nonterm("Expression");
        let expr_tail = gram_gen.new_nonterm("Expression_Tail");
        let term = gram_gen.new_term("Term", "[[:digit:]]+");
        let op = gram_gen.new_term("Operator", "[-/+*]");

        let prod1 = gram_gen.new_prod(expr, vec![term, expr_tail]);
        let prod2 = gram_gen.new_prod(expr_tail, vec![op, expr]);
        gram_gen.make_eps(expr_tail);

        let gram = gram_gen.generate();

        println!("{}", gram);

        assert_eq!(gram.find_next(expr, Some("4")).unwrap().unwrap(), prod1);
        assert_eq!(gram.find_next(expr_tail, Some("+")).unwrap().unwrap(), prod2);
        assert_eq!(gram.find_next(expr_tail, None).unwrap(), None);
    }
}