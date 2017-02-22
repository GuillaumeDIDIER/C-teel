/* Opération x86_64

*/
use std::fmt::Display;
use std::fmt;

// Opérations unaires
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum x64UnaryOp {
    addi(i64),
    sete,
    setne,
    setl,
    setle,
    setg,
    setge,
}

impl Display for x64UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &x64UnaryOp::addi(ival) => {
                write!(f, "add ${}", ival)
            },
            _ => {
                write!(f, "{:?}", self)
            }
        }
    }
}

// Opérations binaires
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum x64BinaryOp {
    mov,
    add,
    sub,
    mul,
    div,
    cmp, // dest - src
    test,// dest & src
}

impl Display for x64BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            _ => {
                write!(f, "{:?}", self)
            }
        }
    }
}

// Operations de branchement
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum x64Branch {
    #[allow(non_camel_case_types)]
    je,
    jne,
    jle,
    jl,
    jge,
    jg,
}

impl Display for x64Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            _ => {
                write!(f, "{:?}", self)
            }
        }
    }
}
