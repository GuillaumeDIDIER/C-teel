use std::fmt;
use std::fmt::Debug;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rdi,
    Rsi,
    Rsp,
    Rbp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Pseudo(i64),
}

impl Register {
    pub fn result() -> Register {
        Register::Rax
    }

    pub fn parameters() -> Vec<Register> {
        vec![Register::Rdi, Register::Rsi, Register::Rdx, Register::Rcx, Register::R8, Register::R9]
    }

    pub fn caller_saved() -> Vec<Register> {
        let mut v = Vec::new();
        v.push(Register::result());
        v.push(Register::R10);
        v.append(& mut Register::parameters());
        v
    }

    pub fn callee_saved() -> Vec<Register> {
        vec![Register::Rbx, Register::R12, /*Register::R13, Register::R14, Register::R15,*/]
    }

    pub fn allocatable() -> Vec<Register> {
        let mut v = Register::caller_saved();
        v.append(& mut Register::callee_saved());
        v
    }

    pub fn tmp() -> (Register, Register) {
        (Register::Rbp, Register::R11)
    }

    pub fn is_pseudo(&self) -> bool {
        match self {
            &Register::Pseudo(_) => true,
            _ => false,
        }
    }

    pub fn is_hardware(&self) -> bool {
        !self.is_pseudo()
    }
}

impl Debug for Register {
    fn fmt(&self,  f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Register::Rax => {return write!(f, "%rax");},
            &Register::Rbx => {return write!(f, "%rbx");},
            &Register::Rcx => {return write!(f, "%rcx");},
            &Register::Rdx => {return write!(f, "%rdx");},
            &Register::Rdi => {return write!(f, "%rdi");},
            &Register::Rsi => {return write!(f, "%rsi");},
            &Register::Rsp => {return write!(f, "%rsp");},
            &Register::Rbp => {return write!(f, "%rbp");},
            &Register::R8  => {return write!(f, "%r8 ");},
            &Register::R9  => {return write!(f, "%r9 ");},
            &Register::R10 => {return write!(f, "%r10");},
            &Register::R11 => {return write!(f, "%r11");},
            &Register::R12 => {return write!(f, "%r12");},
            &Register::R13 => {return write!(f, "%r13");},
            &Register::R14 => {return write!(f, "%r14");},
            &Register::R15 => {return write!(f, "%r15");},
            &Register::Pseudo(i) => {return write!(f, "#{:?}", i);},
        }
    }
}

pub struct RegisterAllocator {
    count: i64,
}


impl RegisterAllocator {
    pub fn new() -> Self {
        RegisterAllocator{count: 0}
    }
    pub fn fresh(& mut self) -> Register {
        let res = Register::Pseudo(self.count);
        self.count += 1;
        res
    }
}
