use rtl::ops;
pub use parse::ast::Ident;
//use std::vec;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::fmt;
use rtl::register::Register;
use rtl::label::Label;

#[derive(Debug)] // Fixme !!!
pub enum Instruction {
    Const(i64, Register, Label),
    AccessGlobal(Ident, Register, Label),
    AssignGlobal(Register, Ident, Label),
    Load(Register, i64, Register, Label),
    Store(Register, Register, i64, Label),
    UnaryOp(ops::x64UnaryOp, Register, Label),
    BinaryOp(ops::x64BinaryOp, Register, Register, Label),
    Branch(ops::x64Branch, Label, Label),
    Call(Ident, usize, Label),
    Goto(Label),
    AllocFrame(Label),
    DeleteFrame(Label),
    GetParam(usize, Register, Label),
    PushParam(Register, Label),
    Return,
}

impl Instruction {
    pub fn successors(&self) -> Vec<Label> { // Fixme !!
        match self {
            &Instruction::Const(_,_, ref label)
            | &Instruction::AccessGlobal(_, _, ref label)
            | &Instruction::AssignGlobal(_, _, ref label)
            | &Instruction::Load(_, _, _, ref label)
            | &Instruction::Store(_, _, _, ref label)
            | &Instruction::UnaryOp(_, _, ref label)
            | &Instruction::BinaryOp(_, _, _, ref label)
            | &Instruction::Call(_, _, ref label)
            | &Instruction::Goto(ref label)
            | &Instruction::AllocFrame(ref label)
            | &Instruction::DeleteFrame(ref label)
            | &Instruction::GetParam(_, _, ref label)
            | &Instruction::PushParam(_, ref label)  => {
                vec![label.clone()]
            },
            &Instruction::Branch(_, ref label1, ref label2) => {
                vec![label2.clone(), label1.clone()]
            },
            &Instruction::Return => {
                Vec::new()
            }
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Instruction::Const(ref value, ref reg, ref label) => {
                write!(f, "mov ${} {} --> {}", value, reg, label)
            },
            &Instruction::AccessGlobal(ref name, ref reg, ref label) => {
                write!(f, "mov {} {} --> {}", name, reg, label)
            },
            &Instruction::AssignGlobal(ref reg, ref name, ref label) => {
                write!(f, "mov {} {} --> {}", reg, name, label)
            },
            &Instruction::Load(ref sreg, ref offset, ref dreg, ref label) => {
                write!(f, "mov {}({}) {} --> {}", offset, sreg, dreg, label)
            },
            &Instruction::Store(ref sreg, ref dreg, ref offset, ref label) => {
                write!(f, "mov {} {}({}) --> {}", sreg, offset, dreg, label)
            },
            &Instruction::UnaryOp(ref op, ref reg, ref label) => {
                write!(f, "{} {} --> {}", op, reg, label)
            },
            &Instruction::BinaryOp(ref op, ref sreg, ref dreg, ref label) => {
                write!(f, "{} {} {} --> {}", op, sreg, dreg, label)
            },
            &Instruction::Call(ref name, ref argc, ref label) => {
                write!(f, "call {}({}) --> {}", name, argc, label)
            },
            &Instruction::Goto(ref label) => {
                write!(f, "goto {}", label)
            },
            &Instruction::Branch(ref branch_op, ref label1, ref label2) => {
                write!(f, "{} --> {}, {}", branch_op, label1, label2)
            },
            &Instruction::AllocFrame(ref label) => {
                write!(f, "alloc_frame --> {}", label)
            },
            &Instruction::DeleteFrame(ref label) => {
                write!(f, "delete_frame --> {}", label)
            },
            &Instruction::GetParam(ref index, ref dest, ref label) => {
                write!(f, "mov stackp({}) {} --> {}", index, dest, label)
            },
            &Instruction::PushParam(ref src, ref label) => {
                write!(f, "push {} --> {}", src, label)
            },
            &Instruction::Return => {
                write!(f, "return")
            },
            //_ => write!(f, "{:#?}\n", self)
        }
    }
}

#[derive(Debug)]
pub struct FuncDefinition {
    pub name: Ident,
    pub formals: usize,
    pub entry: Label,
    pub body: HashMap<Label, Instruction>,
}

impl FuncDefinition {
    fn print_body(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut visited = HashSet::<Label>::new();
        self.visit(&mut visited, self.entry.clone(), f)
    }

    fn visit(& self, visited: & mut HashSet<Label>, l: Label, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if visited.contains(&l) {
            return Ok(());
        }
        visited.insert(l.clone());
        if let Some(instruction) = self.body.get(&l) {
            try!(write!(f, "  {}: {}\n", l, instruction));
            for s in instruction.successors() {
                try!(self.visit(visited, s, f));
            }
            return Ok(())
        } else {
            return Ok(());
        }
    }
}

impl Display for FuncDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "{}({})\n", self.name, self.formals));
        try!(write!(f, "  entry  : {}\n", self.entry));
        self.print_body(f)
    }
}



#[derive(Debug)]
pub struct File {
    pub globals : Vec<Ident>,
    pub functions: Vec<FuncDefinition>,
}

impl Display for File {
    fn fmt(&self,  f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "== ERTL ==================================================\n"));
        for i in 0..self.functions.len() {
            try!(write!(f, "{}", self.functions[i]));
        }
        write!(f, "== END  ==================================================\n")
    }
}
