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
    let mut p = parse::parser::Parser::new();

    let ret = match p.file(" int main( ) { }") {
        (mut p, IResult::Done(_,_)) => 0,
        (mut p, IResult::Incomplete(i)) => {println!("INC {:?}", i);1},
        (mut p, IResult::Error(e)) => {println!("{:?}", e);1},
    };
    println!("{:?}", ret);


}
