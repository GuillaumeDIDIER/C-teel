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
    match parse::lexer::convert_char("a"){
        Some(a) => println!("'a' {}", a),
        _ => println!("Argh"),
    }
    match parse::lexer::integer(" '\\\'' "){ IResult::Done(rem, ast::Expression::Int(i)) => println!("{:?} {:?}", i, rem), _ => println!("Argh", ),}
    //assert!(match parse::lexer::integer("'a'"){ IResult::Done(rem, ast::Expression::Int(i)) => i == 97 && rem == &""[..], _ => false});

        let ret = match parse::lexer::integer("'\x7f'") {
            IResult::Done(_,_) => 0,
            IResult::Incomplete(_) => {println!("INC");1},
            IResult::Error(e) => {println!("ERR, {}", e);1},
        };
        println!("{:?}", ret);
        exit(ret);

}
