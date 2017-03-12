use rtl::ops;
pub use parse::ast::Ident;
//use std::vec;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::fmt;
use common::register::Register;
use rtl::label::Label;
use rtl::label::LabelAllocator;
use common::register::RegisterAllocator;
//use rtl::label;

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
    Call(Register, Ident, Vec<Register>, Label),
    Goto(Label),
}

impl Instruction {
    pub fn successors(&self) -> Vec<Label>{ // Fixme !!
        match *self {
            Instruction::Const(_,_, ref label)
            | Instruction::AccessGlobal(_, _, ref label)
            | Instruction::AssignGlobal(_, _, ref label)
            | Instruction::Load(_, _, _, ref label)
            | Instruction::Store(_, _, _, ref label)
            | Instruction::UnaryOp(_, _, ref label)
            | Instruction::BinaryOp(_, _, _, ref label)
            | Instruction::Call(_, _, _, ref label)
            | Instruction::Goto(ref label) => {
                vec![label.clone()]
            },
            Instruction::Branch(_, ref label1, ref label2) => {
                vec![label2.clone(), label1.clone()]
            }
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Instruction::Const(ref value, ref reg, ref label) => {
                write!(f, "mov ${} {} --> {}", value, reg, label)
            },
            Instruction::AccessGlobal(ref name, ref reg, ref label) => {
                write!(f, "mov {} {} --> {}", name, reg, label)
            },
            Instruction::AssignGlobal(ref reg, ref name, ref label) => {
                write!(f, "mov {} {} --> {}", reg, name, label)
            },
            Instruction::Load(ref sreg, ref offset, ref dreg, ref label) => {
                write!(f, "mov {}({}) {} --> {}", offset, sreg, dreg, label)
            },
            Instruction::Store(ref sreg, ref dreg, ref offset, ref label) => {
                write!(f, "mov {} {}({}) --> {}", sreg, offset, dreg, label)
            },
            Instruction::UnaryOp(ref op, ref reg, ref label) => {
                write!(f, "{} {} --> {}", op, reg, label)
            },
            Instruction::BinaryOp(ref op, ref sreg, ref dreg, ref label) => {
                write!(f, "{} {} {} --> {}", op, sreg, dreg, label)
            },
            Instruction::Call(ref reg, ref name, ref args, ref label) => {
                try!(write!(f, "{} <- call {}(", reg, name));
                let mut i = args.iter(); // Refactor this into an auxilair function
                if let Some(arg0) = i.next() {
                    try!(write!(f, "{}", arg0));
                    for arg in i {
                        try!(write!(f, ", {}", arg));
                    }
                }
                write!(f, ") --> {}", label)
            },
            Instruction::Goto(ref label) => {
                write!(f, "goto {}", label)
            },
            Instruction::Branch(ref branch_op, ref label1, ref label2) => {
                write!(f, "{} --> {}, {}", branch_op, label1, label2)
            },
            //_ => write!(f, "{:#?}\n", self)
        }
    }
}

#[derive(Debug)]
pub struct FuncDefinition {
    pub name: Ident,
    pub formals: Vec<Register>,
    pub result: Register,
    pub locals: HashSet<Register>,
    pub entry: Label,
    pub exit: Label,
    pub body: HashMap<Label, Instruction>,
    pub label_allocator: LabelAllocator,
    pub register_allocator: RegisterAllocator,
}

impl FuncDefinition {
    fn print_body(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut visited = HashSet::<Label>::new();
        return self.visit(&mut visited, self.entry.clone(), f);
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
        try!(write!(f, "{} {}(", self.result, self.name));
        let mut i = self.formals.iter();
        if let Some(formal0) = i.next() {
            try!(write!(f, "{}", formal0));
            for formal in i {
                try!(write!(f, ", {}", formal));
            }
        }
        try!(write!(f, ")\n"));
        try!(write!(f, "  entry  : {}\n", self.entry));
        try!(write!(f, "  exit   : {}\n", self.exit));
        try!(write!(f, "  locals : "));
        let mut i = self.locals.iter();
        if let Some(local0) = i.next() {
            try!(write!(f, "{}", local0));
            for local in i {
                try!(write!(f, ", {}", local));
            }
        }
        try!(write!(f, "\n"));
        return self.print_body(f);
    }

}

#[derive(Debug)]
pub struct File {
    pub globals : Vec<Ident>,
    pub functions: Vec<FuncDefinition>,
}


impl Display for File {
    fn fmt(&self,  f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "== RTL ==================================================\n"));
        for i in 0..self.functions.len() {
            try!(write!(f, "{}", self.functions[i]));
        }
        write!(f, "== END ==================================================\n")
    }
}
