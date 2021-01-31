use crate::defines::{keywords_map_init, Token, TokenType};
use std::collections::HashMap;

pub struct Lexer {
    source: Vec<String>,
    line: usize,
    ptr: usize,
    pub next_token_info: TokenType,
    next_token: Token,
    key_words: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer {
            source: source
                .split('\n')
                .map(|s| {
                    if s.chars().next() == Some('/')
                        && (s.chars().nth(1) == Some('/') || s.chars().nth(1) == Some('*'))
                    {
                        return "".to_string();
                    }
                    s.to_string()
                })
                // .filter(|s| !(s.is_empty() | (s.as_str() == "\r")))
                .collect(),
            line: 0,
            ptr: 0,
            next_token_info: TokenType::None,
            next_token: Token(TokenType::None, 0, None),
            key_words: keywords_map_init(),
        }
    }

    pub fn get_line(&self) -> String {
        format!("{}:{} ### {}", self.line, self.ptr, self.source[self.line])
    }

    fn scan_name(&self, s: &str, start: usize) -> (usize, String) {
        let codes = &s[start..];
        let mut i = 0;
        for c in codes.chars() {
            match c {
                '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => i += 1,
                _ => break,
            }
        }
        (i, codes[..i].to_string())
    }

    fn scan_string(&self, s: &str, start: usize) -> (usize, String) {
        let codes = &s[start..];
        let mut i = 0;
        for c in codes.chars() {
            match c {
                '"' => {
                    i += 1;
                    return (i, codes[..i - 1].to_string());
                }
                _ => i += 1,
            }
        }
        panic!("scan_string: \" not matched!");
    }

    fn scan_num(&self, s: &str, start: usize) -> (usize, String) {
        let codes = &s[start - 1..];
        let mut i = 0;
        for c in codes.chars() {
            match c {
                '0'..='9' => i += 1,
                _ => return (i - 1, codes[..i].to_string()),
            }
        }
        panic!(
            "scan_num: Not a num! `{}`\n Error in {}: {}\n",
            codes,
            self.line + 1,
            self.source[self.line]
        )
    }

    fn erase_comment(&self, s: &str, start: usize) -> (bool, usize) {
        let codes = &s[start..];
        let mut chars = codes.chars();
        let mut i = 0;
        let head = chars.next();

        if head == Some('/') {
            for c in chars {
                i += 1;
                if c == '\n' {
                    return (true, i);
                }
            }
        } else if head == Some('*') {
            let mut last = chars.next().unwrap();
            for c in chars {
                i += 1;
                if c == '/' && last == '*' {
                    return (false, i + 1);
                } else if c == '\n' {
                    return (false, i);
                }
                last = c;
            }
        } else {
            return (true, 0);
        }
        (false, i)
    }

    pub fn get_next_token(&mut self) -> Token {
        if self.next_token_info != TokenType::None {
            self.next_token_info = TokenType::None;
            return self.next_token.clone();
        }

        if self.ptr >= self.source[self.line].len() {
            self.line += 1;
            while self.source[self.line].is_empty() {
                self.line += 1;
                if self.line >= self.source.len() {
                    return Token(TokenType::Eof, self.line, None);
                }
            }
            self.ptr = 0;
        }

        let mut current_line = self.source[self.line][self.ptr..].chars();
        self.ptr += 1;
        let c = current_line.next().unwrap();
        match c {
            '{' => Token(TokenType::LeftBrace, self.line, Some("{".to_string())),
            '}' => Token(TokenType::RightBrace, self.line, Some("}".to_string())),

            '[' => Token(TokenType::LeftMBracket, self.line, Some("[".to_string())),
            ']' => Token(TokenType::RightMBracket, self.line, Some("]".to_string())),

            '(' => Token(TokenType::LeftBracket, self.line, Some("(".to_string())),
            ')' => Token(TokenType::RightBracket, self.line, Some(")".to_string())),

            '.' => Token(TokenType::Dot, self.line, Some(".".to_string())),
            ',' => Token(TokenType::Comma, self.line, Some(",".to_string())),
            ';' => Token(TokenType::Semicolon, self.line, Some(";".to_string())),

            '+' => Token(TokenType::Plus, self.line, Some("+".to_string())),
            '-' => Token(TokenType::Minus, self.line, Some("-".to_string())),
            '*' => Token(TokenType::Multi, self.line, Some("*".to_string())),
            '/' => {
                let (isdivide, i) = self.erase_comment(&self.source[self.line], self.ptr);
                if isdivide {
                    Token(TokenType::Divide, self.line, Some("/".to_string()))
                } else {
                    self.ptr += i + 1;
                    self.get_next_token()
                }
            }

            '&' => Token(TokenType::And, self.line, Some("&amp;".to_string())),
            '|' => Token(TokenType::Or, self.line, Some("|".to_string())),
            '>' => Token(TokenType::Gt, self.line, Some("&gt;".to_string())),
            '<' => Token(TokenType::Lt, self.line, Some("&lt;".to_string())),

            '=' => Token(TokenType::Equal, self.line, Some("=".to_string())),
            '~' => Token(TokenType::Not, self.line, Some("~".to_string())),

            '"' => {
                let (i, s) = self.scan_string(&self.source[self.line], self.ptr);
                self.ptr += i;
                Token(TokenType::StringConstant, self.line, Some(s))
            }

            '0'..='9' => {
                let (i, num) = self.scan_num(&self.source[self.line], self.ptr);
                self.ptr += i;
                Token(TokenType::IntegerConstant, self.line, Some(num))
            }

            '_' | 'A'..='z' => {
                let (i, s) = self.scan_name(&self.source[self.line], self.ptr - 1);
                self.ptr += i - 1;
                if let Some(token_type) = self.key_words.get(&s) {
                    Token(*token_type, self.line, Some(s))
                } else {
                    Token(TokenType::Identifier, self.line, Some(s))
                }
            }

            ' ' | '\n' | '\r' | '\t' => self.get_next_token(),

            _ => panic!(
                "MatchToken(): unexpected symbol {} {}:{} | {}",
                c,
                self.line,
                self.ptr,
                self.source[self.line].len()
            ),
        }
    }

    pub fn next_token_is(&mut self, token_type: TokenType) -> Token {
        let now_type = self.get_next_token();
        if token_type != now_type.0 {
            panic!(
                "next_tokenIs(): expect {:?} found {:?} in {}: {}\n",
                token_type,
                now_type,
                self.line + 1,
                self.source[self.line]
            );
        }
        now_type
    }

    pub fn lookahead(&mut self) -> Token {
        let now_type = self.get_next_token();
        self.next_token = now_type.clone();
        self.next_token_info = now_type.0;
        now_type
    }

    pub fn print_tokens(&mut self) -> String {
        let mut res = String::from("<tokens>\n");
        let mut tok = self.get_next_token();
        loop {
            match tok.0 {
                TokenType::Eof => break,
                _ => {
                    res += format!("{}", tok).as_str();
                    tok = self.get_next_token();
                }
            }
        }
        self.line = 0;
        self.ptr = 0;
        res + "</tokens>"
    }

    pub fn output_tokens(&mut self, file: String) -> std::io::Result<()> {
        std::fs::write(file, self.print_tokens())
    }

    pub fn print_source(&self) {
        println!("Source: ##################### ");
        println!("{:?}", self.source);
        println!("End:    ##################### ");
    }
}

#[test]
fn aa() {
    let a = "0absdcase0";
    let mut ch = a.chars();
    ch.next();
    for c in ch {
        assert_ne!(c, '0');
    }
}
