pub mod ast;
pub mod defines;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused)]
    pub jack
);

#[allow(unused_assignments)]
pub fn preprocess(s: String) -> String {
    use CommentState::*;
    let mut stat = NoComm;
    let mut start = 0;
    let mut end = 0;

    let mut res = String::new();
    let cs = s.as_str().as_bytes();

    let n = cs.len();
    let mut iter = cs.iter().enumerate();

    while let Some((idx, c)) = iter.next() {
        match stat {
            Slash => {
                if *c == '\n' as u8 {
                    stat = NoComm;
                    start = idx + 1;
                }
            }
            Star => {
                if *c == '*' as u8 && idx + 1 < n && cs[idx + 1] == '/' as u8 {
                    stat = NoComm;
                    iter.next();
                    start = idx + 2;
                }
            }
            NoComm => {
                if *c == '/' as u8 && idx + 1 < n {
                    if cs[idx + 1] == '/' as u8 {
                        end = idx;
                        stat = Slash;
                        if end > start {
                            res.push_str(std::str::from_utf8(&cs[start..end]).unwrap());
                        }
                    } else if cs[idx + 1] == '*' as u8 {
                        end = idx;
                        stat = Star;
                        if end > start {
                            res.push_str(std::str::from_utf8(&cs[start..end]).unwrap());
                        }
                    }
                }
            }
        }
    }

    match stat {
        NoComm => {
            res.push_str(std::str::from_utf8(&cs[start..n]).unwrap());
        }
        _ => {}
    }

    res
}

pub fn parse_class_decl(file_name: &str) -> crate::ast::ClassDec {
    use std::io::prelude::*;
    let mut file = std::fs::File::open(file_name).unwrap();
    let mut contents = String::new();
    let _ = file.read_to_string(&mut contents);

    contents = preprocess(contents);

    println!("{}\n\n", contents);

    match jack::ClassDeclParser::new().parse(contents.as_str()) {
        Ok(class) => class,
        Err(e) => panic!("{:?}", e),
    }
}

#[derive(Clone, Copy)]
enum CommentState {
    Slash,
    Star,
    NoComm,
}
