extern crate C_teel;
extern crate nom;
extern crate clap;

use C_teel::parse;
use C_teel::parse::ast;
use nom::IResult;
use clap::{Arg, App};
use std::io::prelude::*;
use std::fs::File;
use std::process::exit;

//use std::boxed;

fn main(){
    let p = parse::parser::Parser::new();

    let ret = match p.file(" int main( ) { }") {
        (p, IResult::Done(_,_)) => 0,
        (p, IResult::Incomplete(i)) => {println!("INC {:?}", i);1},
        (p, IResult::Error(e)) => {println!("{:?}", e);1},
    };
    println!("{:?}", ret);


}
