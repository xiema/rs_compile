use regex::Regex;
use anyhow::{Result, anyhow, Context};

pub type TokenTypeId = i32;

#[derive(Clone)]
pub struct Token {
    pub text: String,
    pub token_type: TokenTypeId,

    pub line_num: usize,
    pub line_pos: usize,
    pub char_pos: usize,
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
    pub fn new(text: &str, token_type: TokenTypeId, line_num: usize, line_pos: usize, char_pos: usize) -> Self {
        Self {
            text: String::from(text),
            token_type: token_type,
            line_num: line_num,
            line_pos: line_pos,
            char_pos: char_pos,
        }
    }
}

pub struct Tokenizer {
    re_ignore: TokenMatcher,
    token_matchers: Vec<TokenMatcher>,
    preprocessor: Option<fn(&str) -> String>,
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

    #[inline]
    fn get_line_info(text: &str) -> (usize, usize) {
        let mut line_pos = 0;
        let num_lines = text.chars().fold(0, 
            |acc, c|
                if c == '\n' {
                    line_pos = 0;
                    acc+1
                }
                else {
                    line_pos += 1;
                    acc
                }
        );
        (num_lines, line_pos)
    }

    pub fn new(tok: Vec<TokenPattern>, ign: TokenPattern, preprocessor: Option<fn(&str) -> String>) -> Self {
        Self {
            re_ignore: Tokenizer::make_re(&ign),
            token_matchers: tok.iter().map(|s| Tokenizer::make_re(s)).collect(),
            preprocessor: preprocessor,
        }
    }

    pub fn tokenize(&mut self, in_str: &str) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        let processed_str = match self.preprocessor {
            None => String::from(in_str),
            Some(f) => f(in_str),
        };

        let mut pos = 0;
        let mut line_num = 0;
        let mut line_pos = 0;

        'main: loop {
            if pos >= processed_str.len() { break Ok(()) }

            // match tokens
            for (i, matcher) in self.token_matchers.iter().enumerate() {
                match matcher.find(&processed_str[pos..]) {
                    Some((text, adv)) => {
                        pos += adv;
                        tokens.push(Token::new(text.as_str(), i as TokenTypeId, line_num, line_pos, pos));
                        let (line_count, new_line_pos) = Self::get_line_info(text.as_str());
                        line_num += line_count;
                        if line_count > 0 { line_pos = new_line_pos; } else { line_pos += adv; }
                        continue 'main;
                    },
                    None => ()
                }
            }

            // match ignore
            match &self.re_ignore.find(&processed_str[pos..]) {
                Some((text, adv)) => {
                    pos += adv;
                    let (line_count, new_line_pos) = Self::get_line_info(text.as_str());
                    line_num += line_count;
                    if line_count > 0 { line_pos = new_line_pos; } else { line_pos += adv; }
                    continue 'main;
                },
                None => ()
            }

            break Err(anyhow!("Unrecognized character: '{}'", &processed_str[pos..pos+1]))

        }.with_context(|| format!("Tokenization Error at {}:{}", line_num, line_pos))?;

        // TODO: end with marker?
        tokens.push(Token::new("", -1, line_num, line_pos, pos));

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
            TokenPattern::Single("[[:space:]&&[^\n]]+"),
        // Preprocessor
            Some(|s| String::from(s.trim()))
        );

        let code = "\n \n\n   line 1, still line 1   \n   line 2//comment here\n  \n  line 3 still line/*multi-line comment 1\nmulti-line comment 2 \\*/  */\n\n  oops   line 4 now // comment with \\\nescape\n\n\" between  \\\" quotes  \\\\\"\n\"more quotes\"";

        let tokens = tokenizer.tokenize(code).unwrap();   
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
        TokenPattern::Single("[[:space:]&&[^\n]]+"),
            None
        );

        let code = "\n \n\n   line 1, still line 1   \n   line 2\n  \n  line 3 still line\n\n  oops   line 4 now";
        
        let res = tokenizer.tokenize(code);
        let e = res.err().unwrap();
        println!("[DISPLAY] {:#}", e);
    }
}