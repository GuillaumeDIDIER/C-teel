extern crate C_teel;

use C_teel::parse;
use C_teel::parse::ast::*;

fn main() {
    let e : UnaryOp = UnaryOp::Not;
    match e {
        UnaryOp::Not => println!("Not"),
        UnaryOp::Minus => println!("Minus"),
    }
    println!("Hello, world!");
}
