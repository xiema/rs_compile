use regex::Regex;
use std::{collections::{HashSet, HashMap}, fmt::Display};

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

pub struct Grammar {
    pub nodes: Vec<NodeDef>,

    ll_flag: bool,
    lr_flag: bool,
    node_id_map: HashMap<String, NodeDefId>,
}

impl Grammar {
    pub fn new() -> Self {
        let gram = Self {
            nodes: Vec::new(),
            
            ll_flag: false,
            lr_flag: false,
            node_id_map: HashMap::new(),
        };
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
            self.lr_flag = true;
        }

        // Right-recursion flag
        if rhs[rhs.len()-1] == def_id {
            self.ll_flag = true;
        }

        self.nodes[def_id].productions.push(rhs);

        new_prod_id
    }

    pub fn is_ll(&self) -> bool {
        self.ll_flag
    }

    pub fn is_lr(&self) -> bool {
        self.lr_flag
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
        let mut gram = Grammar::new();
        
        let expr = gram.new_nonterm("Expression");
        let expr_tail = gram.new_nonterm("Expression_Tail");
        let term = gram.new_term("Term", "[[:digit:]]+");
        let op = gram.new_term("Operator", "[-/+*]");

        let prod1 = gram.new_prod(expr, vec![term, expr_tail]);
        let prod2 = gram.new_prod(expr_tail, vec![op, expr]);
        gram.make_eps(expr_tail);

        println!("{}", gram);

        assert_eq!(gram.find_next(expr, Some("4")).unwrap().unwrap(), prod1);
        assert_eq!(gram.find_next(expr_tail, Some("+")).unwrap().unwrap(), prod2);
        assert_eq!(gram.find_next(expr_tail, None).unwrap(), None);
    }
}