extern crate C_teel;

use C_teel::parse;
use C_teel::parse::ast;

fn main() {
    let e : ast::UnaryOp = ast::UnaryOp::Not;
    match e {
        ast::UnaryOp::Not => println!("Not"),
        ast::UnaryOp::Minus => println!("Minus"),
    }
    println!("Hello, world!");
}
