use std::collections::HashMap;
use std::fmt;

#[macro_export]
macro_rules! hashmap {
    ($( $key:expr => $value:expr ),* ) => {
        {
            let mut map = HashMap::new();
            $(map.insert(($key).to_string(), $value);)*
            map
        }
    };
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenType {
    // Helper
    Eof,
    // Ignored,
    None,
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
    // Symbels
    LeftBrace,
    RightBrace,
    LeftMBracket,
    RightMBracket,
    LeftBracket,
    RightBracket,
    Dot,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Multi,
    Divide,
    And,
    Or,
    Gt,
    Lt,
    Equal,
    Not,
    // Constants
    IntegerConstant,
    StringConstant,
    Identifier,
}

pub fn keywords_map_init() -> HashMap<String, TokenType> {
    hashmap!(
        "class" => TokenType::Class,
        "constructor" => TokenType::Constructor,
        "function" => TokenType::Function,
        "method" => TokenType::Method,
        "field" => TokenType::Field,
        "static" => TokenType::Static,
        "var" => TokenType::Var,
        "int" => TokenType::Int,
        "char" => TokenType::Char,
        "boolean" => TokenType::Boolean,
        "void" => TokenType::Void,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "null" => TokenType::Null,
        "this" => TokenType::This,
        "let" => TokenType::Let,
        "do" => TokenType::Do,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "while" => TokenType::While,
        "return" => TokenType::Return
    )
}

#[derive(Clone, Debug)]
pub struct Token(pub TokenType, pub usize, pub Option<String>);

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            TokenType::Class
            | TokenType::Constructor
            | TokenType::Function
            | TokenType::Method
            | TokenType::Field
            | TokenType::Static
            | TokenType::Var
            | TokenType::Int
            | TokenType::Char
            | TokenType::Boolean
            | TokenType::Void
            | TokenType::True
            | TokenType::False
            | TokenType::Null
            | TokenType::This
            | TokenType::Let
            | TokenType::Do
            | TokenType::If
            | TokenType::Else
            | TokenType::While
            | TokenType::Return => writeln!(f, "<keyword> {} </keyword>", self.2.clone().unwrap()),

            TokenType::LeftBrace
            | TokenType::RightBrace
            | TokenType::LeftMBracket
            | TokenType::RightMBracket
            | TokenType::LeftBracket
            | TokenType::RightBracket
            | TokenType::Dot
            | TokenType::Comma
            | TokenType::Semicolon
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Multi
            | TokenType::Divide
            | TokenType::And
            | TokenType::Or
            | TokenType::Gt
            | TokenType::Lt
            | TokenType::Equal
            | TokenType::Not => writeln!(f, "<symbol> {} </symbol>", self.2.clone().unwrap()),

            TokenType::Identifier => {
                writeln!(f, "<identifier> {} </identifier>", self.2.clone().unwrap())
            }

            TokenType::IntegerConstant => writeln!(
                f,
                "<integerConstant> {} </integerConstant>",
                self.2.clone().unwrap()
            ),

            TokenType::StringConstant => writeln!(
                f,
                "<stringConstant> {} </stringConstant>",
                self.2.clone().unwrap()
            ),
            _ => write!(f, ""),
        }
    }
}

#[test]
fn key() {
    let a = keywords_map_init();
    let cl = a.get("class");
    let co = a.get("constructor");

    assert_eq!(Some(&TokenType::Class), cl);
    assert_eq!(Some(&TokenType::Constructor), co);
}
