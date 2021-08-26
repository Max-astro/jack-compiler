// use crate::backend::Statement;
use crate::defines::*;
use crate::lexer::*;
use crate::symbel::*;
use std::collections::HashSet;
use std::fmt;

#[derive(Debug)]
pub struct Name {
    pub line: usize,
    pub name: String,
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(Variable: {})", self.name)
    }
}

// #[derive(Debug)]
// pub struct Assignment {
//     pub line: usize,
//     pub var: Variable,
//     pub string: String,
// }

// impl fmt::Display for Assignment {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "(Assignment: {})", self.string)
//     }
// }

#[derive(Debug)]
pub struct Class {
    pub line: usize,
    pub class_name: Name,
    pub var_dec: Vec<ClassVarDec>,
    pub indent: usize,
}

#[derive(Debug)]
pub struct ClassVarDec {
    pub line: usize,
    pub class_name: Name,
    pub var_dec: Vec<ClassVarDec>,
}

#[derive(Debug)]
pub struct Type {
    pub line: usize,
    pub class_name: Name,
    pub var_dec: Vec<ClassVarDec>,
}

macro_rules! pushtok {
    ( $self:ident, $tok:expr  ) => {
        $self.ast.push(format!("{}", $tok));
    };
}

fn is_operator(tok: &Token) -> bool {
    match tok.0 {
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Multi
        | TokenType::Divide
        | TokenType::And
        | TokenType::Or
        | TokenType::Gt
        | TokenType::Lt
        | TokenType::Equal => true,
        _ => false,
    }
}

fn is_keyword_const(tok: &Token) -> bool {
    match tok.0 {
        TokenType::True | TokenType::False | TokenType::Null | TokenType::This => true,
        _ => false,
    }
}

fn is_unary_op(tok: &Token) -> bool {
    match tok.0 {
        TokenType::Minus | TokenType::Not => true,
        _ => false,
    }
}

pub struct Parser {
    lex: Lexer,
    tables: SymbelTable,
    subroutines: HashSet<String>,
    ast: Vec<String>,
}

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        Parser {
            lex,
            tables: SymbelTable::new(),
            subroutines: HashSet::new(),
            ast: Vec::new(),
        }
    }

    pub fn output_ast(&mut self, file: String) -> std::io::Result<()> {
        self.parse_class();
        let ast: String = self.ast.iter().flat_map(|s| s.chars()).collect();
        std::fs::write(file, ast)
    }

    fn add_var(&mut self, name: &String, vtype: &String, kind: &Kind) {
        self.tables
            .define(name.clone(), vtype.clone(), kind.clone());
    }

    fn add_subroutine(&mut self, tok: &Token) {
        if let Some(subroutine_name) = tok.2.clone() {
            if !self.subroutines.insert(subroutine_name.clone()) {
                panic!(
                    "add_subroutine(): Subroutine Name: `{}` aleady exist. token: `{:?}`\n",
                    subroutine_name, tok
                )
            }
        }
    }

    fn is_type(&self, tok: &Token) -> bool {
        match tok {
            Token(TokenType::Int, _, _) => true,
            Token(TokenType::Char, _, _) => true,
            Token(TokenType::Boolean, _, _) => true,
            Token(TokenType::Identifier, _, _) => true,
            _ => false,
        }
    }

    fn get_type(&self, tok: &Token) -> String {
        match tok {
            Token(TokenType::Int, _, _) => "int".to_string(),
            Token(TokenType::Char, _, _) => "char".to_string(),
            Token(TokenType::Boolean, _, _) => "bool".to_string(),
            Token(TokenType::Identifier, _, Some(class_name)) => class_name.clone(),
            _ => panic!("get_type: token {:?} not a type", tok),
        }
    }

    fn is_var(&self, tok: &Token) -> bool {
        match tok {
            // Token(TokenType::Identifier, _, Some(var_name)) => self.variables.contains(var_name),
            Token(TokenType::Identifier, _, _) => true,
            _ => false,
        }
    }

    // Book api
    /// class: 'class' className '{' classvarDec* subroutineDec* '}'
    fn parse_class(&mut self) {
        self.ast.push("<class>\n".to_string());

        //parse 'class' className
        pushtok!(self, self.lex.next_token_is(TokenType::Class));
        let class_name_tok = self.lex.next_token_is(TokenType::Identifier);
        let type_name = class_name_tok.2.clone().unwrap();
        pushtok!(self, class_name_tok);

        pushtok!(self, self.lex.next_token_is(TokenType::LeftBrace));
        // parse classvarDec*
        let mut next_tok = self.lex.lookahead();
        while next_tok.0 == TokenType::Static || next_tok.0 == TokenType::Field {
            self.parse_class_vardec(&type_name);
            next_tok = self.lex.lookahead();
        }
        // parse subroutineDec*
        let mut next_tok = self.lex.lookahead();
        while next_tok.0 == TokenType::Constructor
            || next_tok.0 == TokenType::Function
            || next_tok.0 == TokenType::Method
        {
            self.parse_subroutine_dec();
            next_tok = self.lex.lookahead();
        }
        pushtok!(self, self.lex.next_token_is(TokenType::RightBrace));
        self.ast.push("</class>\n".to_string());
    }

    /// classVarDec: ('static' | 'field') type varName (',' varName)* ';'
    fn parse_class_vardec(&mut self, type_name: &String) {
        self.ast.push("<classVarDec>\n".to_string());
        // parse ('static' | 'field')
        let tok = self.lex.get_next_token();
        let kind = match tok.0 {
            TokenType::Static => Kind::Static,
            TokenType::Field => Kind::Field,
            _ => panic!("parse_class_vardec: {:?} not static or field", tok),
        };

        // if tok.0 == TokenType::Static || tok.0 == TokenType::Field {
        //     pushtok!(self, tok);
        // } else {
        //     panic!(
        //         "parse_class_vardec() error in {}, token:`{:?}` is not 'static' or 'field'\n",
        //         tok.1, tok
        //     )
        // }

        // parse type varName
        let tok = self.lex.get_next_token();
        let vtype = self.get_type(&tok);

        let var_tok = self.lex.next_token_is(TokenType::Identifier);
        let name = var_tok.2.unwrap();
        // add var into scope
        self.add_var(&name, &vtype, &kind);

        // if self.is_type(&tok) {
        //     pushtok!(self, tok);
        //     pushtok!(self, self.lex.next_token_is(TokenType::Identifier));
        // } else {
        //     panic!(
        //         "parse_class_vardec() error in {}, token:`{:?}` is not a type\n",
        //         tok.1, tok
        //     )
        // }

        // parse (',' varName)* ';'
        let mut next_tok = self.lex.lookahead();
        while next_tok.0 == TokenType::Comma {
            pushtok!(self, self.lex.next_token_is(TokenType::Comma));
            // pushtok!(self, self.lex.next_token_is(TokenType::Identifier));

            let var_tok = self.lex.next_token_is(TokenType::Identifier);
            let name = var_tok.2.unwrap();
            // add var into scope
            self.add_var(&name, &vtype, &kind);

            next_tok = self.lex.lookahead();
        }
        pushtok!(self, self.lex.next_token_is(TokenType::Semicolon));
        self.ast.push("</classVarDec>\n".to_string());
    }

    /// subroutineDec:
    /// ('constructor' | 'function' | 'method')
    /// ('void' | type) subroutineName '(' parameterList ')'
    /// subroutineBody
    fn parse_subroutine_dec(&mut self) {
        self.ast.push("<subroutineDec>\n".to_string());

        // parse ('constructor' | 'function' | 'method')
        let tok = self.lex.get_next_token();
        match tok.0 {
            TokenType::Constructor | TokenType::Function | TokenType::Method => pushtok!(self, tok),
            _ => panic!(
                "parse_subroutine_dec() error in {}, token:`{:?}` is not a subroutineDec\n",
                tok.1, tok
            ),
        }

        // parse ('void' | type) subroutineName '(' parameterList ')' subroutineBody
        let tok = self.lex.get_next_token();
        if self.is_type(&tok) || tok.0 == TokenType::Void {
            // ('void' | type) subroutineName
            pushtok!(self, tok);
            pushtok!(self, self.lex.next_token_is(TokenType::Identifier));

            // '(' parameterList ')'
            pushtok!(self, self.lex.next_token_is(TokenType::LeftBracket));
            self.parse_parameter_list();
            pushtok!(self, self.lex.next_token_is(TokenType::RightBracket));
        }
        // subroutineBody
        self.parse_subroutine_body();

        self.ast.push("</subroutineDec>\n".to_string());
    }

    /// subroutineBody: '{' varDec* statements '}'
    fn parse_subroutine_body(&mut self) {
        self.ast.push("<subroutineBody>\n".to_string());

        pushtok!(self, self.lex.next_token_is(TokenType::LeftBrace));
        // parse varDec*
        let mut next_tok = self.lex.lookahead();
        while next_tok.0 == TokenType::Var {
            self.parse_var_dec();
            next_tok = self.lex.lookahead();
        }
        // parse statements
        self.parse_statements();
        pushtok!(self, self.lex.next_token_is(TokenType::RightBrace));

        self.ast.push("</subroutineBody>\n".to_string());
    }

    /// subroutineCall: (subroutineName | (className | varName)) '(' expressionList ')'
    fn parse_subroutine_call(&mut self, name_tok: Token) {
        if self.lex.lookahead().0 == TokenType::LeftBracket {
            // if self.subroutines.contains(&name) {
            // parse subroutineName
            pushtok!(self, name_tok);
        } else if self.is_var(&name_tok) || self.is_type(&name_tok) {
            // parse (classname | varName) '.' subroutineName
            pushtok!(self, name_tok);
            pushtok!(self, self.lex.next_token_is(TokenType::Dot));
            pushtok!(self, self.lex.next_token_is(TokenType::Identifier));
        } else {
            panic!(
                "parse_subroutine_call() error in {}, token:`{:?}`\n",
                name_tok.1, name_tok
            );
        }

        // parse  '(' expressionList ')'
        pushtok!(self, self.lex.next_token_is(TokenType::LeftBracket));
        self.parse_expression_list();
        pushtok!(self, self.lex.next_token_is(TokenType::RightBracket));
    }

    /// parameterList: ((type varName) (',' type varName)*)?
    fn parse_parameter_list(&mut self) {
        self.ast.push("<parameterList>\n".to_string());
        // Ignore empty parameter list, ie. next token is ')'
        if self.lex.lookahead().0 != TokenType::RightBracket {
            // parse (type varName)
            let tok = self.lex.get_next_token();
            if self.is_type(&tok) {
                pushtok!(self, tok);
                pushtok!(self, self.lex.next_token_is(TokenType::Identifier));
            }

            // parse (',' type varName)*
            let mut next_tok = self.lex.lookahead();
            while next_tok.0 == TokenType::Comma {
                pushtok!(self, self.lex.next_token_is(TokenType::Comma));
                let tok = self.lex.get_next_token();
                if self.is_type(&tok) {
                    pushtok!(self, tok);
                } else {
                    panic!(
                        "parse_parameter_list() error in `parse (',' type varName)*` token:`{:?}`\n",
                        tok
                    );
                }
                pushtok!(self, self.lex.next_token_is(TokenType::Identifier));
                next_tok = self.lex.lookahead();
            }
        }
        self.ast.push("</parameterList>\n".to_string());
    }

    /// varDec: 'var' type varName (',' varName)* ';'
    fn parse_var_dec(&mut self) {
        self.ast.push("<varDec>\n".to_string());

        // 'var'
        pushtok!(self, self.lex.next_token_is(TokenType::Var));

        // parse type: keywords | class name
        let tok = self.lex.get_next_token();
        let vtype = tok.2.clone().unwrap();
        if self.is_type(&tok) {
            pushtok!(self, tok);
        } else {
            panic!("parse_var() error in {}, token:`{}`\n", tok.1, tok);
        }

        // parse varName
        let varname_tok = self.lex.next_token_is(TokenType::Identifier);
        // add new declared variable into variables_set
        let name = varname_tok.2.clone().unwrap();
        self.add_var(&name, &vtype, &Kind::Var);
        pushtok!(self, varname_tok);

        // parse multiple varName (identifiers)
        while self.lex.lookahead().0 == TokenType::Comma {
            pushtok!(self, self.lex.next_token_is(TokenType::Comma));
            // add new declared variable into variables_set
            let varname_tok = self.lex.next_token_is(TokenType::Identifier);
            let name = varname_tok.2.clone().unwrap();
            self.add_var(&name, &vtype, &Kind::Var);
            pushtok!(self, varname_tok);
        }

        // parse ';'
        pushtok!(self, self.lex.next_token_is(TokenType::Semicolon));
        self.ast.push("</varDec>\n".to_string());
    }

    /// statements: statement*
    fn parse_statements(&mut self) {
        self.ast.push("<statements>\n".to_string());

        let mut tok = self.lex.lookahead();
        while tok.0 != TokenType::RightBrace {
            match tok.0 {
                TokenType::Let => self.parse_let(),
                TokenType::If => self.parse_if(),
                TokenType::While => self.parse_while(),
                TokenType::Do => self.parse_do(),
                TokenType::Return => self.parse_return(),
                _ => {
                    panic!(
                    "parse_statements() unexpected statement: error in {}, token:`{:?}` line: {}\n",
                    tok.1, tok, self.lex.get_line()
                )
                }
            }
            tok = self.lex.lookahead();
        }

        self.ast.push("</statements>\n".to_string());
    }

    /// doStatement: 'do' subroutineCall
    fn parse_do(&mut self) {
        self.ast.push("<doStatement>\n".to_string());

        // parse 'do' subroutineCall ';'
        pushtok!(self, self.lex.next_token_is(TokenType::Do));
        let name_tok = self.lex.next_token_is(TokenType::Identifier);
        self.parse_subroutine_call(name_tok);
        pushtok!(self, self.lex.next_token_is(TokenType::Semicolon));

        self.ast.push("</doStatement>\n".to_string());
    }

    /// letStatenebt: 'let' varName ('[' expression ']')? '=' expression ';'
    fn parse_let(&mut self) {
        self.ast.push("<letStatement>\n".to_string());

        // parse 'let' and varName
        pushtok!(self, self.lex.next_token_is(TokenType::Let));
        pushtok!(self, self.lex.next_token_is(TokenType::Identifier));

        // parse '[' expression ']'?
        let tok = self.lex.lookahead();
        if tok.0 == TokenType::LeftMBracket {
            pushtok!(self, self.lex.next_token_is(TokenType::LeftMBracket));
            self.parse_expression();
            pushtok!(self, self.lex.next_token_is(TokenType::RightMBracket));
        }

        // parse '=' expression ';'
        pushtok!(self, self.lex.next_token_is(TokenType::Equal));
        self.parse_expression();
        pushtok!(self, self.lex.next_token_is(TokenType::Semicolon));

        self.ast.push("</letStatement>\n".to_string());
    }

    /// whileStatement: 'while' '(' expression ')' '{' statements '}'
    fn parse_while(&mut self) {
        self.ast.push("<whileStatement>\n".to_string());

        // parse 'while' '(' expression ')'
        pushtok!(self, self.lex.next_token_is(TokenType::While));
        pushtok!(self, self.lex.next_token_is(TokenType::LeftBracket));
        self.parse_expression();
        pushtok!(self, self.lex.next_token_is(TokenType::RightBracket));

        // parse '{' statements '}'
        pushtok!(self, self.lex.next_token_is(TokenType::LeftBrace));
        self.parse_statements();
        pushtok!(self, self.lex.next_token_is(TokenType::RightBrace));
        self.ast.push("</whileStatement>\n".to_string());
    }

    /// returnStatement: 'return' expression? ';'
    fn parse_return(&mut self) {
        self.ast.push("<returnStatement>\n".to_string());

        // parse 'return'
        pushtok!(self, self.lex.next_token_is(TokenType::Return));

        // parse expression?
        let tok = self.lex.lookahead();
        if tok.0 != TokenType::Semicolon {
            self.parse_expression();
        }

        // parse ';'
        pushtok!(self, self.lex.next_token_is(TokenType::Semicolon));
        self.ast.push("</returnStatement>\n".to_string());
    }

    /// ifStatement: 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
    fn parse_if(&mut self) {
        self.ast.push("<ifStatement>\n".to_string());

        // parse 'if' '(' expression ')'
        pushtok!(self, self.lex.next_token_is(TokenType::If));
        pushtok!(self, self.lex.next_token_is(TokenType::LeftBracket));
        self.parse_expression();
        pushtok!(self, self.lex.next_token_is(TokenType::RightBracket));

        // parse '{' statements '}'
        pushtok!(self, self.lex.next_token_is(TokenType::LeftBrace));
        self.parse_statements();
        pushtok!(self, self.lex.next_token_is(TokenType::RightBrace));

        // parse 'else' '{' statements '}'?
        let tok = self.lex.lookahead();
        if tok.0 == TokenType::Else {
            pushtok!(self, self.lex.next_token_is(TokenType::Else));
            pushtok!(self, self.lex.next_token_is(TokenType::LeftBrace));
            self.parse_statements();
            pushtok!(self, self.lex.next_token_is(TokenType::RightBrace));
        }
        self.ast.push("</ifStatement>\n".to_string());
    }

    /// expression: term (op term)*
    fn parse_expression(&mut self) {
        self.ast.push("<expression>\n".to_string());

        // parse term
        self.parse_term();

        // parse (op term)*
        while is_operator(&self.lex.lookahead()) {
            pushtok!(self, self.lex.get_next_token());
            //println!("\nparse_expression while: {:?}\n", self.lex.lookahead());
            self.parse_term();
        }

        self.ast.push("</expression>\n".to_string());
    }

    /// expressionList: (expression (',' expression)*)?
    fn parse_expression_list(&mut self) {
        self.ast.push("<expressionList>\n".to_string());

        // Ignore empty expression list, ie. next token is ')'
        if self.lex.lookahead().0 != TokenType::RightBracket {
            // parse expression
            self.parse_expression();
            // parse (',' expression)*
            let mut next_tok = self.lex.lookahead();
            while next_tok.0 == TokenType::Comma {
                pushtok!(self, self.lex.next_token_is(TokenType::Comma));
                self.parse_expression();
                next_tok = self.lex.lookahead();
            }
        }

        self.ast.push("</expressionList>\n".to_string());
    }

    /// term: integerConstant | stringConstant | keywordConstant
    /// | varName | varName '[' expression ']'
    /// | subroutineCall
    /// | '(' expression ')'
    /// | unaryOp term
    fn parse_term(&mut self) {
        self.ast.push("<term>\n".to_string());
        let tok = self.lex.get_next_token();

        match tok.0 {
            // parse integerConstant | stringConstant
            TokenType::IntegerConstant | TokenType::StringConstant => pushtok!(self, tok),
            // parse subroutineCall | varName | varName '[' expression ']'
            TokenType::Identifier => {
                let next_tok = self.lex.lookahead();
                if next_tok.0 == TokenType::LeftBracket || next_tok.0 == TokenType::Dot {
                    // parse subroutineCall
                    // println!(
                    //     "\nbefore subroutine_call {:?} ### NEXT TOK: {:?}\n",
                    //     tok, next_tok
                    // );
                    self.parse_subroutine_call(tok);
                } else {
                    // println!("\nIn term-VarName {:?} ### NEXT TOK: {:?}\n", tok, next_tok);
                    // parse varName | varName '[' expression ']'
                    if self.is_var(&tok) {
                        pushtok!(self, tok);
                        if next_tok.0 == TokenType::LeftMBracket {
                            pushtok!(self, self.lex.next_token_is(TokenType::LeftMBracket));
                            self.parse_expression();
                            pushtok!(self, self.lex.next_token_is(TokenType::RightMBracket));
                        }
                    } else {
                        panic!("parse_term(): token `{:?}` variable not defined\n", tok);
                    }
                }
            }
            // parse '(' expression ')'
            TokenType::LeftBracket => {
                pushtok!(self, tok);
                self.parse_expression();
                pushtok!(self, self.lex.next_token_is(TokenType::RightBracket));
            }
            // parse keywordConstant | unaryOp term
            _ => {
                // println!("\nIn term-match default {:?}\n", tok);
                if is_keyword_const(&tok) {
                    // parse keywordConstant
                    pushtok!(self, tok);
                } else if is_unary_op(&tok) {
                    // parse unaryOp term
                    pushtok!(self, tok);
                    self.parse_term();
                } else {
                    panic!("parse_term(): unexpected token `{:?}`\n", tok);
                }
            }
        }

        self.ast.push("</term>\n".to_string());
    }
}
