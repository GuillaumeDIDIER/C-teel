use RTL::ops;
pub use parse::ast::Ident;
//use std::vec;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fmt;
use RTL::register::Register;
use RTL::label::Label;
use RTL::label;

#[derive(Debug)] // Fixme !!!
pub enum Instruction {
    Const(i64, Register, Label),
    AcessGlobal(Ident, Register, Label),
    AssignGlobal(Register, Ident, Label),
    Load(Register, i64, Register, Label),
    Store(Register, Register, i64, Label),
    UnaryOp(ops::x64UnaryOp, Register, Label),
    BinaryOp(ops::x64BinaryOp, Register, Register, Label),
    UnaryBranch(ops::x64UnaryBranch, Register, Label, Label),
    BinaryBranch(ops::x64BinaryBranch, Register, Register, Label, Label),
    Call(Register, Ident, Vec<Register>, Label),
    Goto(Label),
}

impl Instruction {
    pub fn successors(&self) -> Vec<Label>{
        return Vec::new();
    }
}

pub struct FuncDefinition {
    pub name: Ident,
    pub formals: Vec<Register>,
    pub result: Register,
    pub locals: HashSet<Register>,
    pub entry: Label,
    pub exit: Label,
    pub body: HashMap<Label, Instruction>,
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
            let res = write!(f, "  {:#?}: {:#?}", l, instruction);
            if res.is_err() {
                return res;
            }
            for s in instruction.successors() {
                let res = self.visit(visited, s, f);
                if res.is_err() {
                    return res;
                }
            }
            return Ok(())
        } else {
            return Ok(());
        }
    }
}

impl Debug for FuncDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let res = write!(f, "{:#?} {:#?}{:#?}", self.result, self.name, self.formals);
        if res.is_err() {
            return res;
        }
        let res = write!(f, "  entry  : {:#?}", self.entry);
        if res.is_err() {
            return res;
        }
        let res = write!(f, "  exit   : {:#?}", self.exit);
        if res.is_err() {
            return res;
        }
        let res = write!(f, "  locals : {:#?}", self.locals);
        if res.is_err() {
            return res;
        }
        return self.print_body(f);
    }

}


pub struct File {
    pub globals : Vec<Ident>,
    pub functions: Vec<FuncDefinition>,
}


impl Debug for File {
    fn fmt(&self,  f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let res = write!(f, "== RTL ==================================================\n");
        if res.is_err() {
            return res;
        }
        for i in 0..self.functions.len() {
            let res = write!(f, "{:#?}", self.functions[i]);
            if res.is_err() {
                return res;
            }
        }
        Ok(())
    }
}
