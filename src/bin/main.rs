extern crate C_teel;
extern crate nom;
extern crate clap;

use C_teel::parse;
//use C_teel::parse::ast;
use nom::IResult;
use clap::{Arg, App};
use std::io::prelude::*;
use std::fs::File;
use std::process::exit;
//use std::boxed;

fn main(){
    let matches = App::new("C-teel")
                              .version("0.0.1")
                              .author("guillaume.didier@polytechnique.edu")
                              .about("Compiles Badly a subset of C to x86_64")
                              .arg(Arg::with_name("INPUT")
                                   .help("Sets the input file to use")
                                   .required(true)
                                   .index(1))
                              .arg(Arg::with_name("parse-only")
                                   .long("parse-only")
                                   .help("Only parse"))
                              .get_matches();
    let input = matches.value_of("INPUT").unwrap();
    println!("Using input file: {}", input);


    if let Ok(mut f) = File::open(input) {
        let mut s = String::new();
        if f.read_to_string(&mut s).is_err() { exit(-2) }
        let p = parse::parser::Parser::new();

        let ret = match p.file(&s) {
            (p, IResult::Done(rem, _)) => {println!("{:?}, {:?}", p.location.line, rem);0},
            (p, IResult::Incomplete(_)) => {println!("INC {:?}", p.location.line);1},
            (p, IResult::Error(e)) => {println!("{:?} {:?}", p.location.line, e);1},
        };
        println!("{:?}", ret);
        exit(ret);
        }
    else{
        exit(-1);
    }
}
