use regex::Regex;

#[derive(Debug, Clone)]
pub enum TokenizationErr {
    UnrecognizedChar(usize),
}

enum State {
    START,
    TOKEN,
    IGNORE,
}

pub type TokenTypeId = i32;

#[derive(Clone)]
pub struct Token {
    pub text: String,
    pub token_type: TokenTypeId,
}

impl Token {
    pub fn new(text: &str, token_type: TokenTypeId) -> Self {
        Self {
            text: String::from(text),
            token_type: token_type,
        }
    }
}

pub struct Tokenizer {
    re_ignore: Regex,
    token_matchers: Vec<Regex>,
    pos: usize,
    state: State,
}

impl Tokenizer {
    pub fn make_re(s: &str) -> Regex {
        Regex::new(format!("^({})", s).as_str()).unwrap()
    }

    pub fn new(tok: Vec<&str>, ign: &str) -> Self {
        Self {
            re_ignore: Regex::new(Tokenizer::make_re(ign).as_str()).unwrap(),
            token_matchers: tok.iter().map(|s| Tokenizer::make_re(s)).collect(),
            pos: 0,
            state: State::START,
        }
    }

    fn match_state(&mut self, next: State, seq: &str, tokens: &mut Vec<Token>) -> bool {
        match next {
            State::START => panic!("Can't transition to START"),
            State::TOKEN => {
                for i in 0..self.token_matchers.len() {
                    if let Some(m) = self.token_matchers[i].find(seq) {
                        self.pos += m.end();
                        tokens.push(Token::new(&seq[m.start()..m.end()], i as TokenTypeId));
                        return true;
                    }
                }
            },
            State::IGNORE => {
                if let Some(m) = self.re_ignore.find(seq) {
                    self.pos += m.end();
                    return true;
                }
            },
        };
        return false;
    }

    pub fn tokenize(&mut self, in_str: &str) -> Result<Vec<Token>, TokenizationErr> {
        let mut tokens: Vec<Token> = Vec::new();
        let re_ws1 = Regex::new("^[[:space:]]*").unwrap();
        let re_ws2 = Regex::new("[[:space:]]*$").unwrap();
        let start = match re_ws1.find(in_str) {
            Some(m) => m.end(),
            None => 0,
        };
        let end = match re_ws2.find(in_str) {
            Some(m) => m.start(),
            None => in_str.len(),
        };

        self.pos = start;

        while self.pos < end {
            let ok = match self.state {
            State::START => self.match_state(State::TOKEN, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::IGNORE, &in_str[self.pos..], &mut tokens)
            ,
            State::TOKEN => self.match_state(State::TOKEN, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::IGNORE, &in_str[self.pos..], &mut tokens)
            ,
            State::IGNORE => self.match_state(State::TOKEN, &in_str[self.pos..], &mut tokens)
            };
            if !ok {
                return Err(TokenizationErr::UnrecognizedChar(self.pos));
            }
        }

        // TODO: end with marker?
        tokens.push(Token::new("", -1));

        return Ok(tokens);
    }
}


#[allow(unused_variables)]
#[cfg(test)]
mod tests {
    use super::*;

    fn assert_tokens_str(t: &[Token], ts: Vec<&str>) {
        assert_eq!(t.len(), ts.len());
        for i in 0..t.len() {
            assert_eq!(t[i].text, ts[i]);
        }
    }

    #[test]
    fn tokenizer_test() {
        let mut tokenizer = Tokenizer::new(
            vec!["\n+[[:space:]]*", "[[:^space:]&&[^,]]+", ","], 
            "[[:space:]&&[^\n]]+");

        let code = "\n \n\n   line 1, still line 1   \n   line 2\n  \n  line 3 still line\n\n  oops   line 4 now";

        let tokens = tokenizer.tokenize(code).unwrap();
        
        // display_tokens(&tokens);
        
        assert_tokens_str(&tokens[0..6], vec!["line", "1", ",", "still", "line", "1"]);
        assert_eq!(tokens[6].token_type, 0 as TokenTypeId);
        assert_tokens_str(&tokens[7..9], vec!["line", "2"]);
        assert_eq!(tokens[9].token_type, 0 as TokenTypeId);
        assert_tokens_str(&tokens[10..14], vec!["line", "3", "still", "line"]);
        assert_eq!(tokens[14].token_type, 0 as TokenTypeId);
        assert_tokens_str(&tokens[15..19], vec!["oops", "line", "4", "now"]);
    }

    #[test]
    fn tokenizer_unhandled_char() {
        let mut tokenizer = Tokenizer::new(
        vec!["\n+[[:space:]]*", "([[:^space:]&&[^,]]+)"], 
        "[[:space:]&&[^\n]]+");

        let code = "\n \n\n   line 1, still line 1   \n   line 2\n  \n  line 3 still line\n\n  oops   line 4 now";
        
        assert!(matches!(tokenizer.tokenize(code), Err(TokenizationErr::UnrecognizedChar(e)) if e == 13));
    }
}