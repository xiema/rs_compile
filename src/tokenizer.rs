use std::cmp;

use regex::Regex;

enum State {
    START,
    TOKEN,
    IGNORE,
    BLANK,
}

pub struct Tokenizer {
    re_token: Regex,
    re_ignore: Regex,
    re_blank: Regex,
    blank_char: String,

    pos: usize,
    state: State,
}

impl Tokenizer {
    pub fn make_re(s: &str) -> String {
        String::from("^(") + s + ")"
    }

    pub fn new(tok: &str, ign: &str, blk: &str, blank_char: &str) -> Self {
        Self {
            re_token: Regex::new(Tokenizer::make_re(tok).as_str()).unwrap(),
            re_ignore: Regex::new(Tokenizer::make_re(ign).as_str()).unwrap(),
            re_blank: Regex::new(Tokenizer::make_re(blk).as_str()).unwrap(),
            blank_char: String::from(blank_char),

            pos: 0,
            state: State::START,
        }
    }

    fn match_state(&mut self, next: State, seq: &str, tokens: &mut Vec<String>) -> bool {
        let res = match next {
            State::START => panic!("Can't transition to START"),
            State::TOKEN => self.re_token.find(seq),
            State::IGNORE => self.re_ignore.find(seq),
            State::BLANK => self.re_blank.find(seq),
        };
        match res {
            Some(m) => {
                match next {
                    State::TOKEN => tokens.push(String::from(&seq[m.start()..m.end()])),
                    State::BLANK => match tokens.last() {
                        Some(last) => if last.ne(self.blank_char.as_str()) { tokens.push(String::from(self.blank_char.as_str())) },
                        None => () },
                    State::IGNORE => (),
                    State::START => (),
                }
                self.pos = self.pos + m.end();
                self.state = next;
                return true;
            },
            None => return false,
        }
    }

    pub fn tokenize(&mut self, in_str: &str) -> Vec<String> {
        let mut tokens: Vec<String> = Vec::new();

        while self.pos < in_str.len() {
            let ok = match self.state {
            State::START => self.match_state(State::TOKEN, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::IGNORE, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::BLANK, &in_str[self.pos..], &mut tokens)
            ,
            State::TOKEN => self.match_state(State::TOKEN, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::IGNORE, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::BLANK, &in_str[self.pos..], &mut tokens)
            ,
            State::IGNORE => self.match_state(State::TOKEN, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::BLANK, &in_str[self.pos..], &mut tokens)
            ,
            State::BLANK => self.match_state(State::TOKEN, &in_str[self.pos..], &mut tokens)
                            || self.match_state(State::IGNORE, &in_str[self.pos..], &mut tokens)
            };
            if !ok {
                panic!("Tokenization error at char {}:\n{}", self.pos, &in_str[self.pos..cmp::min(in_str.len(), self.pos + 20)]);
            }
        }

        // always end with endl_marker
        tokens.push(String::from(self.blank_char.as_str()));

        return tokens;
    }
}

pub fn display_tokens(tokens: &Vec<String>) {
    for token in tokens {
        if token == "" {
            print!("\n");
        }
        else {
            print!("'{}' ", token);
        }
    }
    print!("\n");
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer_test() {
        let mut tokenizer = Tokenizer::new(
            "[[:^space:]&&[^,]]+|,", 
            "[[:space:]&&[^\n]]+", 
            "\n+([[:space:]]*\n+)*",
            ";");

        let code = "\n \n\n   line 1, still line 1   \n   line 2\n  \n  line 3 still line\n\n  oops   line 4 now";

        let tokens = tokenizer.tokenize(code);
        
        // display_tokens(&tokens);
        
        assert_eq!(tokens[0..7], ["line", "1", ",", "still", "line", "1", ";"]);
        assert_eq!(tokens[7..10], ["line", "2", ";"]);
        assert_eq!(tokens[10..15], ["line", "3", "still", "line", ";"]);
        assert_eq!(tokens[15..20], ["oops", "line", "4", "now", ";"]);
    }

    #[test]
    #[should_panic]
    fn tokenizer_unhandled_char() {
        let mut tokenizer = Tokenizer::new(
        "([[:^space:]&&[^,]]+)", 
        "[[:space:]&&[^\n]]+", 
        "[[:space:]]*\n+[[:space:]]*",
        ";");

        let code = "\n \n\n   line 1, still line 1   \n   line 2\n  \n  line 3 still line\n\n  oops   line 4 now";

        let tokens = tokenizer.tokenize(code);
        
        // display_tokens(&tokens);
    }
}