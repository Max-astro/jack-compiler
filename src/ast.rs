use crate::defines::TokenType;

/// Defines a primitive expression.
#[allow(dead_code)]
#[derive(Debug)]
pub enum Expr {
    Binary {
        op: TokenType,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Unary {
        op: TokenType,
        expr: Box<Expr>,
    },

    IntConst(i32),
    StringConst(String),
    KwConst(TokenType),

    VarName(String),
    ArrElem(String, Box<Expr>),

    SubroutineCall {
        class_type: Option<ClassType>,
        obj_name: Option<String>,
        routine_name: String,
        args: Vec<Box<Expr>>,
    },
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Stmt {
    LetVar(String, Box<Expr>),
    LetArr {
        arr_name: String,
        idx_expr: Box<Expr>,
        rhs: Box<Expr>,
    },
    IfStmt {
        cond: Box<Expr>,
        consequence: Vec<Box<Stmt>>,
        alternative: Option<Vec<Box<Stmt>>>,
    },
    WhileStmt(Box<Expr>, Vec<Box<Stmt>>),
    DoStmt(Box<Expr>),
    ReturnStmt(Option<Box<Expr>>),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum RoutineType {
    Constructor,
    Function,
    Method,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ClassType {
    Void,
    Int,
    Boolean,
    Char,
    Class(String),
}

#[derive(Debug)]
pub struct ClassDec {
    pub name: String,
    pub memb_var: Vec<ClassVarDec>,
    pub subroutines: Vec<SubroutineDec>,
}

#[derive(Debug)]
pub struct ClassVarDec {
    pub field: bool,
    pub ty: ClassType,
    pub vars: Vec<String>,
}

/// Defines the prototype (name and parameters) of a function.
#[derive(Debug)]
pub struct SubroutineDec {
    pub routine_type: RoutineType,
    pub ret_type: ClassType,
    pub routine_name: String,
    pub args: ParamList,
    pub body: SubroutineBody,
}

#[derive(Debug)]
pub struct SubroutineBody {
    pub var_decl: Vec<VarDecl>,
    pub stmt: Vec<Box<Stmt>>,
}

pub type ParamList = Vec<(ClassType, String)>;
pub type VarDecl = (ClassType, Vec<String>);

// pub enum TypedVal {}
// pub enum KeywordConst {
//     True,
//     False,
//     Null,
//     This,
// }
