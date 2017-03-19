use common::label::Label;
use std::collections::HashSet;
use std::fmt::Display;
use std::fmt;



pub struct Output {
    pub functions: Vec<FunctionOutput>,
    pub globals: Vec<GlobalDef>
}

impl Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f,"    .text\n    .globl main\n"));
        for func in self.functions.iter() {
            try!(write!(f, "{}", func));
        }
        try!(write!(f,"    .data\n"));
        for glob in self.globals.iter() {
            try!(write!(f, "{}", glob));
        }
        Ok(())
    }
}

pub struct FunctionOutput {
    pub name: String,
    pub instructions: Vec<(Option<Label>, String)>,
    pub labels: HashSet<Label>,
}

impl Display for FunctionOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "{}:\n", self.name));
        for &(ref label, ref instruction) in self.instructions.iter() {
            if let &Some(l) = label {
                if self.labels.contains(&l) {
                    try!(write!(f, ".{}.{}:\n", self.name, l));
                }
            }
            try!(write!(f, "{}\n", instruction));
        }
        Ok(())
    }
}

pub struct GlobalDef {
    pub name: String,
}

impl Display for GlobalDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}:\n    .quad 0\n", self.name)
    }
}
