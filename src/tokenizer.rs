use regex::Regex;

#[derive(Debug, Clone)]
pub enum TokenizationErr {
    UnrecognizedChar(usize),
}

pub type TokenTypeId = i32;

#[derive(Clone)]
pub struct Token {
    pub text: String,
    pub token_type: TokenTypeId,
}

pub enum TokenPattern<'a> {
    Single(&'a str),
    Surround(&'a str, &'a str, &'a str),
}

enum TokenMatcher {
    Single(Regex),
    Surround(Regex, Regex, Option<Regex>),
}

impl TokenMatcher {
    fn find(&self, seq: &str) -> Option<(String, usize)> {
        match self {
            Self::Single(p) => {
                if let Some(m) = p.find(seq) {
                    return Some((String::from(m.as_str()), m.as_str().len()));
                }
            },
            Self::Surround(a, b, esc) => {
                if let Some(m) = a.find(seq) {
                    let start = m.start();
                    let mut end = m.end();

                    let mut s = String::new();
                    s += m.as_str();

                    while end < seq.len() {
                        if let Some(closing) = b.find(&seq[end..]) {
                            if let Some(esc) = esc {
                                if let Some(escaping) = esc.find(&seq[end..]) {
                                    if escaping.end() < closing.end() {
                                        s += &seq[end..end+escaping.start()];
                                        end += escaping.end();
                                        s += &seq[end..end+1];
                                        end += 1;
                                        continue;
                                    }
                                }
                            }
                            s += &seq[end..end+closing.end()];
                            end += closing.end();
                            return Some((s, end-start));
                        }
                        panic!("Missing closing surround");
                    }
                }
            }
        }
        None
    }
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
    re_ignore: TokenMatcher,
    token_matchers: Vec<TokenMatcher>,
}

impl Tokenizer {
    fn make_re(pat: &TokenPattern) -> TokenMatcher {
        match pat {
            TokenPattern::Single(s) => {
                if s.starts_with("^") {
                    TokenMatcher::Single(Regex::new(s).unwrap())
                }
                else {
                    TokenMatcher::Single(Regex::new(format!("^({})", s).as_str()).unwrap())
                }
            },
            TokenPattern::Surround(a, b, esc) => {
                TokenMatcher::Surround(
                    Regex::new(format!("^({})", a).as_str()).unwrap(),
                    Regex::new(format!("{}", b).as_str()).unwrap(),
                    match esc.len() {
                        0 => None,
                        _ => Some(Regex::new(format!("{}", esc).as_str()).unwrap()),
                    },
                )
            }
        }
    }

    pub fn new(tok: Vec<TokenPattern>, ign: TokenPattern) -> Self {
        Self {
            re_ignore: Tokenizer::make_re(&ign),
            token_matchers: tok.iter().map(|s| Tokenizer::make_re(s)).collect(),
        }
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

        let mut pos = start;

        'main: while pos < end {
            // match tokens
            for (i, matcher) in self.token_matchers.iter().enumerate() {
                match matcher.find(&in_str[pos..]) {
                    Some((tok, adv)) => {
                        pos += adv;
                        tokens.push(Token::new(tok.as_str(), i as TokenTypeId));
                        continue 'main;
                    },
                    None => ()
                }
            }

            // match ignore
            match &self.re_ignore.find(&in_str[pos..]) {
                Some((_, adv)) => {
                    pos += adv;
                    continue 'main;
                },
                None => ()
            }

            return Err(TokenizationErr::UnrecognizedChar(pos));
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

    #[allow(dead_code)]
    fn assert_tokens_str(t: &[Token], ts: Vec<&str>) {
        assert_eq!(t.len(), ts.len());
        for i in 0..t.len() {
            assert_eq!(t[i].text, ts[i]);
        }
    }

    #[test]
    fn tokenizer_test() {
        let mut tokenizer = Tokenizer::new(vec![
        // Tokens
            // whitespace sequence starting with \n
            TokenPattern::Single("\n+[[:space:]]*"),
            // quoted sequence w/ escaping
            TokenPattern::Surround("\"", "\"", "\\\\"),
            // single-line comment w/ escaping
            TokenPattern::Surround("//", "\n", "\\\\"),
            // multi-line comment w/ escaping
            TokenPattern::Surround("/\\*", "\\*/", "\\\\"),
            // single char
            TokenPattern::Single(","),
            // alnum
            TokenPattern::Single("[[:alnum:]]+")
        ],
        // Ignore
            TokenPattern::Single("[[:space:]&&[^\n]]+")
        );

        let code = "\n \n\n   line 1, still line 1   \n   line 2//comment here\n  \n  line 3 still line/*multi-line comment 1\nmulti-line comment 2 \\*/  */\n\n  oops   line 4 now // comment with \\\nescape\n\n\" between  \\\" quotes  \\\\\"\n\"more quotes\"";

        let tokens = match tokenizer.tokenize(code) {
            Ok(t) => t,
            Err(TokenizationErr::UnrecognizedChar(pos)) =>
                panic!("Unrecognized character `{}` at {}:\n`{}`\n", &code[pos..pos+1], pos, &code[pos..std::cmp::min(code.len(), pos+20)])
        };   
        // display_tokens(&tokens);
        
        let check = vec![
            // vec!["\n \n\n   "],
            vec!["line", "1", ",", "still", "line", "1"], vec!["\n   "],
            vec!["line", "2", "//comment here\n"], vec!["\n  "],
            vec!["line", "3", "still", "line", "/*multi-line comment 1\nmulti-line comment 2 */  */"], vec!["\n\n  "],
            vec!["oops", "line", "4", "now", "// comment with \nescape\n"], vec!["\n"],
            vec!["\" between  \" quotes  \\\""], vec!["\n"],
            vec!["\"more quotes\""],
        ];

        assert_eq!(tokens.len(), check.iter().fold(0, |acc, v| acc + v.len()) + 1);

        let mut i = 0;
        for v in check {
            for s in v {
                assert_eq!(tokens[i].text, s);
                i+=1;
            }
        }
        assert_eq!(tokens[tokens.len()-1].token_type, -1 as TokenTypeId);
    }

    #[test]
    fn tokenizer_unhandled_char() {
        let mut tokenizer = Tokenizer::new(vec![
            TokenPattern::Single("\n+[[:space:]]*"),
            TokenPattern::Single("([[:^space:]&&[^,]]+)")
        ], 
        TokenPattern::Single("[[:space:]&&[^\n]]+")
        );

        let code = "\n \n\n   line 1, still line 1   \n   line 2\n  \n  line 3 still line\n\n  oops   line 4 now";
        
        assert!(matches!(tokenizer.tokenize(code), Err(TokenizationErr::UnrecognizedChar(e)) if e == 13));
    }
}