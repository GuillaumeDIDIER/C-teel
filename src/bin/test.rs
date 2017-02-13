extern crate C_teel;
extern crate nom;
extern crate clap;

use C_teel::parse;
//use C_teel::parse::ast;
use nom::IResult;
//use clap::{Arg, App};
//use std::io::prelude::*;
//use std::fs::File;
//use std::process::exit;

//use std::boxed;

fn main(){
    let p = parse::parser::Parser::new();

    let ret = match p.file("int main(){{(a\n);}}") {
        (_, IResult::Done(rem, ast)) => {println!(    "{:?}:{:?}: {:?}", ast[ast.len()-1].stop.line, ast[ast.len()-1].stop.column, rem);0},
        (p, IResult::Incomplete(i)) =>  {println!("INC {:?}:{:?}: {:?}", p.location.line, p.location.column, i)  ;1},
        (p, IResult::Error(e)) =>       {println!("ERR {:?}:{:?}: {:?}", p.location.line, p.location.column, e)  ;1},
    };
    println!("{:?}", ret);


}
