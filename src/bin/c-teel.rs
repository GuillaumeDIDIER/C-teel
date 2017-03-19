// External imports
extern crate C_teel; // NB, for the binaries, the crate is still considered external.
extern crate nom;
extern crate clap;


// Imports
//      Standard library
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;
use std::process::exit;
use std::ffi::OsString;
//      From the parser combinator library
use nom::IResult;
//      Argument parsing
use clap::{Arg, App};
//      The diferrent passes
use C_teel::parse;
use C_teel::typing;
use C_teel::rtl;
use C_teel::ertl;
use C_teel::ltl;
use C_teel::output;

/*
The compilation is controlled from the driver structure.
This structure is initialized with the program args and then the run method is invoqued which does the heavy lifting.

*/

// Self explanatory see Driver::run for more details
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
    // Constructor, which does the argument parsing logix (using clap)
    fn from_args<I, T>(args: I) -> Result<Self, clap::Error> where I: IntoIterator<Item=T>, T: Into<OsString> + Clone {


        // The following logic declares all the arguments to the library.
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
            .version("0.1.0")
            .author("guillaume.didier@polytechnique.edu")
            .about("Compiles Badly a subset of C to x86_64")
            .arg(arg_input)
            .arg(arg_parse_only)
            .arg(arg_type_only)
            .arg(arg_open_only)
            .arg(arg_full);

        // This parses thos arguments and deduce the driver parameters.
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

    // This is where the logic of compilation is.
    fn run(self) -> i32 {
        // Open file
        let src = match File::open(&self.filename) {
            Ok(mut f) => {
                let mut s = String::new();
                if f.read_to_string(&mut s).is_err(){
                    return -3;
                };
                s
            },
            Err(e) => {
                println!("I/O error: {}", e);
                return -2;
            }
        };
        // This is not an interesting mode of oiperation but enables checking if the preceding logic is broken.
        if let Mode::Open = self.mode {
            return 0;
        }

        // Parse file
        let p = parse::parser::Parser::new();

        let past = match p.file(&src) {
            (_, IResult::Done(_, past)) => {past},
            (_, IResult::Incomplete(i)) => {println!("Unexepcted en of input: {:?}", i);return 1;},
            (_, IResult::Error(e)) => {println!("Parse error: {}", e);return 1;},
        };

        // Output and return if in parse-only mode.
        if let Mode::Parse = self.mode {
            println!("{:#?}", past);
            return 0;
        }

        // Type file
        let tast = match typing::ast::File::type_file(past) {
            Ok(t) => t,
            Err(e) => {println!("Type error: {}", e);return 1;},
        };

        // Output and return if in type-only mode.
        if let Mode::Type = self.mode {
            println!("{:#?}", tast);
            return 0;
        }

        // Code generation

        // This pass should not error out
        let rtl_ast = match rtl::File::from_typer_ast(tast) {
            Ok(f) => {f},
            Err(e) => {
                println!("{}", e);
                return 1;
            }
        };

        // the other passes do not error out, but panic should insane condition occur.
        let ertl_ast = ertl::File::from_rtl(rtl_ast);
        let ltl_ast = ltl::File::from_ertl(ertl_ast);
        let output = output::Output::from_ltl(ltl_ast); // This is a vector of (Label, String). The final logic is dieffered in the implementation of the Display::fmt trait.

        // Output to file
        let path = Path::new(&self.filename).with_extension("s");
        let outputfile = File::create(path);
        match outputfile {
            Ok(mut f) => {let res = write!(f, "{}", output);if res.is_err() {-1} else {0} }
            _ => {-1}
        }
    }
}

fn main(){
    // Get an initialized driver or error out
    let driver = match Driver::from_args(std::env::args_os()) {
        Ok(d) => d,
        Err(e) => {
            println!("{}", e);
            exit(-1);
        }
    };
    // run the compilation
    exit(driver.run())
}
