use std::cmp;

use regex::Regex;

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

    pub fn tokenize(&mut self, in_str: &str) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let s = in_str.trim();

        while self.pos < s.len() {
            let ok = match self.state {
            State::START => self.match_state(State::TOKEN, &s[self.pos..], &mut tokens)
                            || self.match_state(State::IGNORE, &s[self.pos..], &mut tokens)
            ,
            State::TOKEN => self.match_state(State::TOKEN, &s[self.pos..], &mut tokens)
                            || self.match_state(State::IGNORE, &s[self.pos..], &mut tokens)
            ,
            State::IGNORE => self.match_state(State::TOKEN, &s[self.pos..], &mut tokens)
            };
            if !ok {
                panic!("Tokenization error at char {}:\n{}", self.pos, &s[self.pos..cmp::min(s.len(), self.pos + 20)]);
            }
        }

        // TODO: end with marker?
        tokens.push(Token::new("", -1));

        return tokens;
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

        let tokens = tokenizer.tokenize(code);
        
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
    #[should_panic]
    fn tokenizer_unhandled_char() {
        let mut tokenizer = Tokenizer::new(
        vec!["\n+[[:space:]]*", "([[:^space:]&&[^,]]+)"], 
        "[[:space:]&&[^\n]]+");

        let code = "\n \n\n   line 1, still line 1   \n   line 2\n  \n  line 3 still line\n\n  oops   line 4 now";
        
        let tokens = tokenizer.tokenize(code);
    }
}