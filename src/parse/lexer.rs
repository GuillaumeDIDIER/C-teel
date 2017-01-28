use parse::ast;
//use std::char;
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

    method!(comment<Parser, &str, &str>, self, alt!(
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

    method!(pub space<Parser, &str, String >, mut self,
        do_parse!(vect: many0!( alt!(call_m!(self.comment) | multispace)) >> ({
            let res = vect.concat();
            let mut v = Vec::new(); v.extend(res.split("\n"));
            self.line += (v.len()) - 1;
            if v.len() == 1 {
                self.column += v[0].len();
            } else {
                self.column = v[v.len() -1].len();
            }
            v.concat()
        }))
    );

    method!(pub space_opt<Parser, &str, Option<String> >, mut self, opt!(call_m!(self.space)) );

    // xkcd://1638
    method!(pub character<Parser, &str, Vec<&str> >, mut self, re_capture!(r#"^('(\\\\|\\"|\\'|[ -\x7F]|(\\x[0-9A-Fa-f][0-9A-Fa-f]))')"#) );


    method!(dec<Parser, &str, &str>, mut self, re_find!(r"^[1-9][0-9]*") );

    method!(hex<Parser, &str, Vec<&str> >, mut self, re_capture!(r"^0x([0-9A-Fa-f]*)") );

    method!(oct<Parser, &str, &str>, mut self, re_find!(r"^0[0-7]*") );

    method!(pub integer<Parser, &str, ast::Expression >, mut self, alt!(
            terminated!(do_parse!(
            decimal: call_m!(self.dec) >>
            value: expr_opt!( decimal.parse::<i64>().ok()) >>
            (ast::Expression::Int(value))
            ), call_m!(self.space_opt))
        |   terminated!(do_parse!(
            hexa: call_m!(self.hex) >>
            value: expr_opt!( i64::from_str_radix(hexa[1], 16).ok() ) >>
            (ast::Expression::Int(value))
            ), call_m!(self.space_opt))
        |   terminated!(do_parse!(
            oct: call_m!(self.oct) >>
            value: expr_opt!(i64::from_str_radix(oct, 8).ok() ) >>
            (ast::Expression::Int(value))
            ), call_m!(self.space_opt))
        |   terminated!(do_parse!(tag!("0") >> (ast::Expression::Int(0))), call_m!(self.space_opt))
        |   terminated!(do_parse!(ch: call_m!(self.character) >> res: expr_opt!(convert_char(ch[2])) >> take_s!(1)>> (ast::Expression::Int(res))), call_m!(self.space_opt)) // It seems tha \x7f confuses the regexp.

    ));



    // Miscellaneous tokens : Comma, parenthesis, braces, ...
    method!(pub comma       <Parser, &str, &str>, mut self, terminated!(tag_s!(","), call_m!(self.space_opt)));
    method!(pub semicolon   <Parser, &str, &str>, mut self, terminated!(tag_s!(";"), call_m!(self.space_opt)));
    method!(pub brace_open  <Parser, &str, &str>, mut self, terminated!(tag_s!("{"), call_m!(self.space_opt)));
    method!(pub brace_close <Parser, &str, &str>, mut self, terminated!(tag_s!("}"), call_m!(self.space_opt)));
    method!(pub parens_open <Parser, &str, &str>, mut self, terminated!(tag_s!("("), call_m!(self.space_opt)));
    method!(pub parens_close<Parser, &str, &str>, mut self, terminated!(tag_s!(")"), call_m!(self.space_opt)));

    // Operators
    method!(pub op_plus     <Parser, &str, &str>, mut self, terminated!(tag_s!("+"), call_m!(self.space_opt)));
    method!(pub op_minus    <Parser, &str, &str>, mut self, terminated!(tag_s!("-"), call_m!(self.space_opt)));
    method!(pub op_star     <Parser, &str, &str>, mut self, terminated!(tag_s!("*"), call_m!(self.space_opt)));
    method!(pub op_div      <Parser, &str, &str>, mut self, terminated!(tag_s!("/"), call_m!(self.space_opt)));
    method!(pub op_not      <Parser, &str, &str>, mut self, terminated!(tag_s!("!"), call_m!(self.space_opt)));

    method!(pub op_deref    <Parser, &str, &str>, mut self, terminated!(tag_s!("->"), call_m!(self.space_opt)));
    method!(pub op_and      <Parser, &str, &str>, mut self, terminated!(tag_s!("&&"), call_m!(self.space_opt)));
    method!(pub op_or       <Parser, &str, &str>, mut self, terminated!(tag_s!("||"), call_m!(self.space_opt)));

    method!(pub op_simple_eq    <Parser, &str, &str>, mut self, terminated!(tag_s!("=" ), call_m!(self.space_opt)));
    method!(pub op_double_eq    <Parser, &str, &str>, mut self, terminated!(tag_s!("=="), call_m!(self.space_opt)));
    method!(pub op_not_eq       <Parser, &str, &str>, mut self, terminated!(tag_s!("!="), call_m!(self.space_opt)));

    method!(pub op_gt   <Parser, &str, &str>, mut self, terminated!(tag_s!(">" ), call_m!(self.space_opt)));
    method!(pub op_lt   <Parser, &str, &str>, mut self, terminated!(tag_s!("<" ), call_m!(self.space_opt)));
    method!(pub op_ge   <Parser, &str, &str>, mut self, terminated!(tag_s!(">="), call_m!(self.space_opt)));
    method!(pub op_le   <Parser, &str, &str>, mut self, terminated!(tag_s!("<="), call_m!(self.space_opt)));


    // Operator groups.
    method!(pub unary_op<Parser, &str, ast::UnaryOp>, mut self, alt!(
        do_parse!(call_m!(self.op_not)   >> (ast::UnaryOp::Not))
        | do_parse!(call_m!(self.op_minus) >> (ast::UnaryOp::Minus))
    ));

    method!(pub binary_op_mul<Parser, &str, ast::BinaryOp>, mut self, alt!(
        do_parse!(call_m!(self.op_star)   >> (ast::BinaryOp::Mult))
        | do_parse!(call_m!(self.op_div) >> (ast::BinaryOp::Div))
    ));

    method!(pub binary_op_arith<Parser, &str, ast::BinaryOp>, mut self, alt!(
        do_parse!(call_m!(self.op_plus)   >> (ast::BinaryOp::Plus))
        | do_parse!(call_m!(self.op_minus) >> (ast::BinaryOp::Minus))
    ));

    method!(pub binary_op_compare<Parser, &str, ast::BinaryOp>, mut self, alt!(
          do_parse!(call_m!(self.op_le)   >> (ast::BinaryOp::LowerEq))
        | do_parse!(call_m!(self.op_ge) >> (ast::BinaryOp::GreaterEq))
        | do_parse!(call_m!(self.op_lt) >> (ast::BinaryOp::Lower))
        | do_parse!(call_m!(self.op_gt) >> (ast::BinaryOp::Greater))
    ));

    method!(pub binary_op_eq<Parser, &str, ast::BinaryOp>, mut self, alt!(
          do_parse!(call_m!(self.op_double_eq)  >> (ast::BinaryOp::Equal))
        | do_parse!(call_m!(self.op_not_eq)     >> (ast::BinaryOp::NotEqual))
    ));

    // Keywords : Do not forget to update check_keyword below.
    method!(pub kwd_int     <Parser, &str, &str>, mut self, terminated!(tag_s!("int"    ), call_m!(self.space_opt)) );
    method!(pub kwd_struct  <Parser, &str, &str>, mut self, terminated!(tag_s!("struct" ), call_m!(self.space_opt)) );
    method!(pub kwd_sizeof  <Parser, &str, &str>, mut self, terminated!(tag_s!("sizeof" ), call_m!(self.space_opt)) );
    method!(pub kwd_if      <Parser, &str, &str>, mut self, terminated!(tag_s!("if"     ), call_m!(self.space_opt)) );
    method!(pub kwd_else    <Parser, &str, &str>, mut self, terminated!(tag_s!("else"   ), call_m!(self.space_opt)) );
    method!(pub kwd_while   <Parser, &str, &str>, mut self, terminated!(tag_s!("while"  ), call_m!(self.space_opt)) );
    method!(pub kwd_return  <Parser, &str, &str>, mut self, terminated!(tag_s!("return" ), call_m!(self.space_opt)) );

    method!(pub identifier <Parser, &str, ast::Ident >, mut self,
        terminated!(
        do_parse!(
            as_str: re_find!(r"^[a-zA-Z_][a-zA-Z_1-9]*") >>
            res: expr_opt!(check_keyword(as_str)) >>
            (res)
        ), call_m!(self.space_opt))
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
