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


    if let Some(mut f) = File::open(input).ok(){
        let mut s = String::new();
        match f.read_to_string(&mut s){
            Err(_) => exit(-2),
            Ok(_) => (),
        }
        let mut p = parse::parser::Parser::new();

        let ret = match p.file(&s) {
            (mut p, IResult::Done(rem, vd)) => {println!("{:?}, {:?}", p.location.line, rem);0},
            (mut p, IResult::Incomplete(_)) => {println!("INC {:?}", p.location.line);1},
            (mut p, IResult::Error(e)) => {println!("{:?} {:?}", p.location.line, e);1},
        };
        println!("{:?}", ret);
        exit(ret);
        }
    else{
        exit(-1);
    }
}
