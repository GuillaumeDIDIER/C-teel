use parse::ast;
use std::char;
//use std::string;
use nom::multispace;
use parse::parser::Parser;




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

/*#[macro_export]
macro_rules! sp (
  ($i:expr, $($args:tt)*) => (
    {
      sep!($i, call_m!(self.space_opt), $($args)*)
    }
  )
);*/

impl Parser {

    method!(comment<Parser, &str, &str>, mut self, alt!(
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

    method!(pub space<Parser, &str, Vec<&str> >, mut self,
        many0!( alt!(call_m!(self.comment) | multispace))
    );

    method!(pub space_opt<Parser, &str, Option<Vec<&str>> >, mut self, opt!(call_m!(self.space)) );

    // xkcd://1638
    method!(pub character<Parser, &str, Vec<&str> >, mut self, re_capture!(r#"^('(\\\\|\\"|\\'|[ -\x7F]|(\\x[0-9A-Fa-f][0-9A-Fa-f]))')"#) );


    method!(dec<Parser, &str, &str>, mut self, re_find!(r"^[1-9][0-9]*") );

    method!(hex<Parser, &str, Vec<&str> >, mut self, re_capture!(r"^0x([0-9A-Fa-f]*)") );

    method!(oct<Parser, &str, &str>, mut self, re_find!(r"^0[0-7]*") );

    method!(pub integer<Parser, &str, ast::Expression >, mut self, alt!(
            preceded!(call_m!(self.space_opt), do_parse!(
            decimal: call_m!(self.dec) >>
            value: expr_opt!( decimal.parse::<i64>().ok()) >>
            (ast::Expression::Int(value))
            ))
        |   preceded!(call_m!(self.space_opt), do_parse!(
            hexa: call_m!(self.hex) >>
            value: expr_opt!( i64::from_str_radix(hexa[1], 16).ok() ) >>
            (ast::Expression::Int(value))
            ))
        |   preceded!(call_m!(self.space_opt), do_parse!(
            oct: call_m!(self.oct) >>
            value: expr_opt!(i64::from_str_radix(oct, 8).ok() ) >>
            (ast::Expression::Int(value))
            ))
        |   preceded!(call_m!(self.space_opt), do_parse!(tag!("0") >> (ast::Expression::Int(0))))
        |   preceded!(call_m!(self.space_opt), do_parse!(ch: call_m!(self.character) >> res: expr_opt!(convert_char(ch[2])) >> take_s!(1)>> (ast::Expression::Int(res)))) // It seems tha \x7f confuses the regexp.

    ));


    method!(pub unary_op<Parser, &str, ast::UnaryOp>, mut self, alt!(
        preceded!(call_m!(self.space_opt), do_parse!(tag_s!("!")   >> (ast::UnaryOp::Not)))
        | preceded!(call_m!(self.space_opt), do_parse!(tag_s!("-") >> (ast::UnaryOp::Minus)))
    ));

    method!(pub binary_op_mul<Parser, &str, ast::BinaryOp>, mut self, alt!(
        preceded!(call_m!(self.space_opt), do_parse!(tag_s!("*")   >> (ast::BinaryOp::Mult)))
        | preceded!(call_m!(self.space_opt), do_parse!(tag_s!("/") >> (ast::BinaryOp::Div)))
    ));

    method!(pub binary_op_arith<Parser, &str, ast::BinaryOp>, mut self, alt!(
        preceded!(call_m!(self.space_opt), do_parse!(tag_s!("+")   >> (ast::BinaryOp::Plus)))
        | preceded!(call_m!(self.space_opt), do_parse!(tag_s!("-") >> (ast::BinaryOp::Minus)))
    ));

    method!(pub binary_op_compare<Parser, &str, ast::BinaryOp>, mut self, alt!(
        preceded!(call_m!(self.space_opt), do_parse!(tag_s!("<=")   >> (ast::BinaryOp::LowerEq)))
        | preceded!(call_m!(self.space_opt), do_parse!(tag_s!(">=") >> (ast::BinaryOp::GreaterEq)))
        | preceded!(call_m!(self.space_opt), do_parse!(tag_s!("<") >> (ast::BinaryOp::Lower)))
        | preceded!(call_m!(self.space_opt), do_parse!(tag_s!(">") >> (ast::BinaryOp::Greater)))
    ));

    method!(pub binary_op_eq<Parser, &str, ast::BinaryOp>, mut self, alt!(
        preceded!(call_m!(self.space_opt), do_parse!(tag_s!("==")   >> (ast::BinaryOp::Equal)))
        | preceded!(call_m!(self.space_opt), do_parse!(tag_s!("!=") >> (ast::BinaryOp::NotEqual)))
    ));


    // Keywords : Do not forget to update check_keyword below.
    method!(pub kwd_int     <Parser, &str, &str>, mut self, preceded!(call_m!(self.space_opt), tag_s!("int")) );
    method!(pub kwd_struct  <Parser, &str, &str>, mut self, preceded!(call_m!(self.space_opt), tag_s!("struct")) );
    method!(pub kwd_sizeof  <Parser, &str, &str>, mut self, preceded!(call_m!(self.space_opt), tag_s!("sizeof")) );
    method!(pub kwd_if      <Parser, &str, &str>, mut self, preceded!(call_m!(self.space_opt), tag_s!("if")) );
    method!(pub kwd_else    <Parser, &str, &str>, mut self, preceded!(call_m!(self.space_opt), tag_s!("else")) );
    method!(pub kwd_while   <Parser, &str, &str>, mut self, preceded!(call_m!(self.space_opt), tag_s!("while")) );
    method!(pub kwd_return  <Parser, &str, &str>, mut self, preceded!(call_m!(self.space_opt), tag_s!("return")) );

    method!(pub identifier <Parser, &str, ast::Ident >, mut self,
        preceded!(call_m!(self.space_opt),
        do_parse!(
            as_str: re_find!(r"^[a-zA-Z_][a-zA-Z_1-9]*") >>
            res: expr_opt!(check_keyword(as_str)) >>
            (res)
        ))
    );

}
// Identifiers
fn check_keyword(id: &str) -> Option<String> {
    let kwds = vec!["int", "struct", "sizeof", "if", "else", "while", "return"];
    if kwds.contains(&id) {
        None
    } else {
        Some(String::new() + id)
    }
}
