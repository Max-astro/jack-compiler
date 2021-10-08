use std::fs::File;
use std::io::prelude::*;

use jack_compiler::{jack, preprocess};

fn main() {}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;

    #[test]
    fn test_ClassDec() {
        let mut file =
            File::open("./jack_codes/10/ArrayTest/Main.jack").unwrap();
        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents);

        contents = preprocess(contents);

        println!("{}\n\n", contents);

        match jack::ClassDeclParser::new().parse(contents.as_str()) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => println!("{:?}", e),
        }
    }

    #[test]
    fn test_VarDecl() {
        let s1 = "var Array a;";
        let s2 = "var int i, sum;";

        match jack::VarDeclParser::new().parse(s1) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }

        match jack::VarDeclParser::new().parse(s2) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }
    }

    #[test]
    fn test_SubroutineDecl() {
        let s1 = "function void main() {
            var int i, sum;
            let a = Array.new(length);
        }";
        // let s2 = "var int i, sum;";

        match jack::SubroutineDeclParser::new().parse(s1) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }

        // match jack::VarDeclParser::new().parse(s2) {
        //     Ok(expr) => println!("{:?}", expr),
        //     Err(e) => println!("{:?}", e),
        // }
    }

    #[test]
    fn test_SubroutineCall() {
        let s1 = "Array.new(length)";

        match jack::SubroutineCallParser::new().parse(s1) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }
    }

    #[test]
    fn test_Expr() {
        let s1 = "a = Array.new(length)";

        match jack::ExprParser::new().parse(s1) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }

        let s2 = "a = 3 * (2 + 1)";
        match jack::ExprParser::new().parse(s2) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }

        let s3 = "a = 3 * 2 + 1";
        match jack::ExprParser::new().parse(s3) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }

        let s4 = "a = (a + ((b / 3) * 2) + 1)";
        match jack::ExprParser::new().parse(s4) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }
    }

    #[test]
    fn test_LetStatement() {
        let s1 = "let i = i + 1;";
        match jack::LetStatementParser::new().parse(s1) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }

        let s1 = "let a[i] = Keyboard.readInt(\"ENTER THE NEXT NUMBER: \");";
        match jack::LetStatementParser::new().parse(s1) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }
    }

    #[test]
    fn test_StringLit() {
        let s1 = "\"abcd\"";
        println!("{:?}", jack::StringConstParser::new().parse(s1).unwrap());
    }

    #[test]
    fn test_Statements() {
        let s1 = r#"while (i < length) {
            let sum = sum + a[i];
            let i = i + 1;
        }
        do Output.printString("THE AVERAGE IS: ");
        if (i) {
            let s = i;
            let s = j;
            let a[i] = j;
        }
        else {
            let i = i;
            let j = j;
            let i = i | j;
        }
        return;
        "#;
        match jack::StatementsParser::new().parse(s1) {
            Ok(expr) => println!("{:?}", expr),
            Err(e) => panic!("{:?}", e),
        }
    }
}
