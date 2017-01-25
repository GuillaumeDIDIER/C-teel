extern crate C_teel;
extern crate nom;

use C_teel::parse;
use C_teel::parse::ast;
use nom::IResult;
//use std::boxed;

fn main() {
    assert!(match parse::lexer::integer("0"){ IResult::Done(rem, ast::Expression::Int(i)) => i == 0 && rem == &""[..], _ => false});
    assert!(match parse::lexer::integer("1"){ IResult::Done(rem, ast::Expression::Int(i)) => i == 1 && rem == &""[..], _ => false});
    assert!(match parse::lexer::integer("10"){ IResult::Done(rem, ast::Expression::Int(i)) => i == 10 && rem == &""[..], _ => false});
    assert!(match parse::lexer::integer("0x10"){ IResult::Done(rem, ast::Expression::Int(i)) => i == 16 && rem == &""[..], _ => false});
    assert!(match parse::lexer::integer("011"){ IResult::Done(rem, ast::Expression::Int(i)) => i == 9 && rem == &""[..], _ => false});
    //let res = match parse::lexer::identifier(" a1 "){
    //    IResult::Done(rem, i) => i == "a1" && rem == &""[..],
    //    _ => false
    //};
    //assert!(res);
    match parse::parser::statement("{int a; a; return a;}") {
        IResult::Done(_,_) => println!("OK"),
        IResult::Incomplete(_) => println!("INC"),
        _ => println!("BAD", ),
    }
}
