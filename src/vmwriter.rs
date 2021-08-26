use std::fmt;
#[derive(Debug, PartialEq)]
pub enum MemorySegment {
    Constant,
    Argument,
    Local,
    Static,
    This,
    That,
    Pointer,
    Temp,
}

impl fmt::Display for MemorySegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::MemorySegment::*;
        write!(
            f,
            "{}",
            match *self {
                Constant => "constant",
                Argument => "argument",
                Local => "local",
                Static => "static",
                This => "this",
                That => "that",
                Pointer => "pointer",
                Temp => "temp",
            }
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum VmInstruction {
    Push(MemorySegment, usize), // segment, index
    Pop(MemorySegment, usize),  // segment, index
    Goto(String),               // label
    IfGoto(String),             // label
    Label(String),              // label (duh)
    Function(String, usize),    // name, local_count
    Call(String, usize),        // name, num_args
    Return,
    Add,
    Sub,
    And,
    Or,
    Lt,
    Gt,
    Eq,
    Not,
    Neg,
}

impl fmt::Display for VmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::VmInstruction::*;
        match *self {
            Push(ref segment, index) => write!(f, "push {} {}", segment, index),
            Pop(ref segment, index) => write!(f, "pop {} {}", segment, index),
            Goto(ref label) => write!(f, "goto {}", label),
            IfGoto(ref label) => write!(f, "if-goto {}", label),
            Label(ref label) => write!(f, "label {}", label),
            Function(ref name, local_count) => write!(f, "function {} {}", name, local_count),
            Call(ref name, num_args) => write!(f, "call {} {}", name, num_args),
            Return => write!(f, "return"),
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Lt => write!(f, "lt"),
            Gt => write!(f, "gt"),
            Eq => write!(f, "eq"),
            Not => write!(f, "not"),
            Neg => write!(f, "neg"),
        }
    }
}

struct VmWriter {
    vm: Vec<String>,
}

impl VmWriter {
    pub fn new() -> Self {
        VmWriter { vm: Vec::new() }
    }

}
