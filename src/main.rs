use clap::{App, Arg};
mod backend;
mod defines;
mod lexer;
mod parser;
use backend::*;
use defines::*;
use lexer::*;
use parser::*;
use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = App::new("MyProgram")
        .author("Max")
        .about("Jack compiler frontend")
        .arg(
            Arg::new("print_source")
                .short('p')
                .about("print source codes"),
        )
        .arg(
            Arg::new("INPUT")
                .about("input jack source code")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("token")
                .short('t')
                .value_name("token_xml")
                .about("output token xml")
                .takes_value(true),
        )
        .arg(
            Arg::new("ast")
                .short('a')
                .value_name("ast_xml")
                .about("output ast xml")
                .takes_value(true),
        )
        .get_matches();

    if let Some(file) = matches.value_of("INPUT") {
        let source = fs::read_to_string(file)?;
        let mut lex = Lexer::new(source);

        if matches.is_present('p') {
            lex.print_source();
        }

        if let Some(token_xml) = matches.value_of("token") {
            lex.output_tokens(token_xml.to_string())?;
        }
        if let Some(ast_xml) = matches.value_of("ast") {
            let mut parser = Parser::new(lex);
            parser.output_ast(ast_xml.to_string())?;
        }
    }

    Ok(())
}
