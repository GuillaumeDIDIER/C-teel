use parse::ast;
use std::char;
//use std::string;
use nom::multispace;

named!(comment<&str, &str>,alt!(
    do_parse!(
        tag_s!("/*")                  >>
        comment_txt: take_until_s!("*/")          >>
        tag_s!("*/")                 >>
        (comment_txt)
    )
    | do_parse!(tag_s!("//") >>
        comment_txt: take_until_s!("\n")          >>
        tag_s!("\n")                 >>
        (comment_txt)
))
);

named!(space<&str, Vec<&str> >,
    many0!( alt!(comment | multispace))
);

named!(pub space_opt<&str, Option<Vec<&str>> >, opt!( space) );

#[macro_export]
macro_rules! sp (
  ($i:expr, $($args:tt)*) => (
    {
      sep!($i, space_opt, $($args)*)
    }
  )
);


named!(dec<&str, &str>, re_find!(r"^[1-9][0-9]*") );

named!(hex<&str, Vec<&str> >, re_capture!(r"^0x([0-9A-Fa-f]*)") );

named!(oct<&str, &str>, re_find!(r"^0[0-7]*") );

// xkcd://1638
named!(pub character<&str, Vec<&str> >, re_capture!(r#"^('(\\\\|\\"|\\'|[ -\x7F]|(\\x[0-9A-Fa-f][0-9A-Fa-f]))')"#) );

pub fn convert_char(ch: &str)->Option<i64>{
    if ch.len() == 1{
        if let Some(c) = ch.chars().next() {

                return Some(c as i64);

        }
        return None
    } else if ch.len() == 2 {
        return match ch {
            "\\\\" => Some(92),
            "\\\'" => Some(39),
            "\\\"" => Some(34),
            _ => None
        };
    } else if ch.len() == 4 {
        return i64::from_str_radix(&ch[2..], 16).ok();
    }

    return None
}

named!(pub integer<&str, ast::Expression >, alt!(
        sp!(do_parse!(
        decimal: dec >>
        value: expr_opt!( decimal.parse::<i64>().ok()) >>
        (ast::Expression::Int(value))
        ))
    |   sp!(do_parse!(
        hexa: hex >>
        value: expr_opt!( i64::from_str_radix(hexa[1], 16).ok() ) >>
        (ast::Expression::Int(value))
        ))
    |   sp!(do_parse!(
        oct: oct >>
        value: expr_opt!(i64::from_str_radix(oct, 8).ok() ) >>
        (ast::Expression::Int(value))
        ))
    |   sp!(do_parse!(tag!("0") >> (ast::Expression::Int(0))))
    |   sp!(do_parse!(ch: character >> res: expr_opt!(convert_char(ch[2])) >> take_s!(1)>> (ast::Expression::Int(res)))) // It seems tha \x7f confuses the regexp.

));

named!(pub unary_op<&str, ast::UnaryOp>, alt!(
    sp!(do_parse!(tag_s!("!")   >> (ast::UnaryOp::Not)))
    | sp!(do_parse!(tag_s!("-") >> (ast::UnaryOp::Minus)))
));

named!(pub binary_op_mul<&str, ast::BinaryOp>, alt!(
    sp!(do_parse!(tag_s!("*")   >> (ast::BinaryOp::Mult)))
    | sp!(do_parse!(sp!(tag_s!("/")) >> (ast::BinaryOp::Div)))
));

named!(pub binary_op_arith<&str, ast::BinaryOp>, alt!(
    sp!(do_parse!(tag_s!("+")   >> (ast::BinaryOp::Plus)))
    | sp!(do_parse!(tag_s!("-") >> (ast::BinaryOp::Minus)))
));

named!(pub binary_op_compare<&str, ast::BinaryOp>, alt!(
    sp!(do_parse!(tag_s!("<=")   >> (ast::BinaryOp::LowerEq)))
    | sp!(do_parse!(tag_s!(">=") >> (ast::BinaryOp::GreaterEq)))
    | sp!(do_parse!(tag_s!("<") >> (ast::BinaryOp::Lower)))
    | sp!(do_parse!(tag_s!(">") >> (ast::BinaryOp::Greater)))
));

named!(pub binary_op_eq<&str, ast::BinaryOp>, alt!(
    sp!(do_parse!(tag_s!("==")   >> (ast::BinaryOp::Equal)))
    | sp!(do_parse!(tag_s!("!=") >> (ast::BinaryOp::NotEqual)))
));


// Keywords :
named!(pub kwd_int <&str, &str>, sp!(tag_s!("int")) );
named!(pub kwd_struct <&str, &str>, sp!(tag_s!("struct")) );
named!(pub kwd_sizeof <&str, &str>, sp!(tag_s!("sizeof")) );
named!(pub kwd_if <&str, &str>, sp!(tag_s!("if")) );
named!(pub kwd_else <&str, &str>, sp!(tag_s!("else")) );
named!(pub kwd_while <&str, &str>, sp!(tag_s!("while")) );
named!(pub kwd_return <&str, &str>, sp!(tag_s!("return")) );

// Identifiers
fn check_keyword(id: &str) -> Option<String> {
    let kwds = vec!["int", "struct", "sizeof", "if", "else", "while", "return"];
    if kwds.contains(&id) {
        None
    } else {
        Some(String::new() + id)
    }
}

named!(pub identifier <&str, ast::Ident >,
    sp!(do_parse!(
        as_str: re_find!(r"^[a-zA-Z_][a-zA-Z_1-9]*") >>
        res: expr_opt!(check_keyword(as_str)) >>
        (res)
    ))
);
