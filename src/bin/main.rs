extern crate C_teel;
extern crate nom;
extern crate clap;

use C_teel::parse;
use C_teel::typing;
//use C_teel::parse::ast;
use nom::IResult;
use clap::{Arg, App};
use std::io::prelude::*;
use std::fs::File;
use std::process::exit;
use std::ffi::OsString;
use C_teel::RTL;
//use std::boxed;

enum Mode {
    Open,
    Parse,
    Type,
    Full,
}

struct Driver {
    mode: Mode,
    filename: String,
}

impl Driver {
    fn from_args<I, T>(args: I) -> Result<Self, clap::Error> where I: IntoIterator<Item=T>, T: Into<OsString> + Clone {

        let arg_input = Arg::with_name("INPUT")
            .help("Name of the source file to use")
            .required(true)
            .index(1);
        let arg_parse_only = Arg::with_name("parse-only")
            .long("parse-only")
            .help("Only parse the source file");
        let arg_type_only = Arg::with_name("type-only")
            .long("type-only")
            .help("Only parse and type the source file")
            .conflicts_with("parse-only");
        let arg_open_only = Arg::with_name("open-only")
            .long("open-only")
            .help("Only open the source file (sanity check of the compiler io)")
            .conflicts_with_all(&["parse-only", "type-only"]);
        let arg_full = Arg::with_name("full")
                .long("full")
                .help("Fully compiles the source file")
                .conflicts_with_all(&["parse-only", "type-only", "open-only"]);
        let app = App::new("C-teel")
            .version("0.0.1")
            .author("guillaume.didier@polytechnique.edu")
            .about("Compiles Badly a subset of C to x86_64")
            .arg(arg_input)
            .arg(arg_parse_only)
            .arg(arg_type_only)
            .arg(arg_open_only)
            .arg(arg_full);




        let args_matches: clap::ArgMatches = match app.get_matches_from_safe(args) {
            Err(e) => {return Err(e);}
            Ok(m) => m
        };
        let input = args_matches.value_of("INPUT").unwrap();
        let mode;
        if args_matches.is_present("open-only") {
            mode = Mode::Open;
        } else if args_matches.is_present("parse-only"){
            mode = Mode::Parse;
        } else if args_matches.is_present("type-only"){
            mode = Mode::Type;
        } else {
            mode = Mode::Full;
        }

        Ok(Driver{mode: mode, filename: String::from(input)})
    }

    fn run(self) -> i32 {
        // Open file
        let src = match File::open(self.filename) {
            Ok(mut f) => {
                let mut s = String::new();
                if f.read_to_string(&mut s).is_err(){
                    return -3;
                };
                s
            },
            Err(e) => {
                println!("{:?}", e);
                return -2;
            }
        };
        if let Mode::Open = self.mode {
            return 0;
        }
        // Parse file
        let p = parse::parser::Parser::new();

        let past = match p.file(&src) {
            (_, IResult::Done(_, past)) => {past},
            (_, IResult::Incomplete(i)) => {println!("{:?}", i);return 1;},
            (_, IResult::Error(e)) => {println!("{:?}", e);return 1;},
        };
        if let Mode::Parse = self.mode {
            return 0;
        }
        // Type file
        let tast = match typing::ast::File::type_file(past) {
            Ok(t) => t,
            Err(e) => {println!("{:?}", e);return 1;},
        };
        if let Mode::Type = self.mode {
            println!("{:#?}", tast);
            return 0;
        }
        let res = RTL::rtltree::File::from_typer_ast(tast);
        match res {
            Ok(f) => {
                println!("{}", f);
                println!("\n{:#?}", f);
            },
            Err(e) => {
                println!("{:?}", e);
                return 1;
            }
        }
        // Finish...

        return 0;
    }
}

fn main(){
    let driver = match Driver::from_args(std::env::args_os()) {
        Ok(d) => d,
        Err(e) => {
            println!("{:?}", e);
            exit(-1);
        }
    };
    exit(driver.run())
}
