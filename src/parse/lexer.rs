use parse::ast;
use parse::ast::*;
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

    method!(comment<Parser, &str, String>, self, alt!(
        do_parse!(
            tag_s!("/*")                  >>
            comment_txt: take_until_s!("*/")          >>
            tag_s!("*/")                 >>
            (String::new() + "/*" + comment_txt + "*/")
        )
        | do_parse!(beg: tag_s!("//") >>
            comment_txt: take_until_s!("\n")          >>
            end: tag_s!("\n")                 >>
            (String::new() + beg + comment_txt + end)
    ))
    );

    method!(pub space<Parser, &str, String >, mut self,
        do_parse!(vect: many0!( alt!(call_m!(self.comment) | map!(multispace, |s: &str|{String::new() + s}))) >> ({
            let res = vect.concat();
            let mut v = Vec::new(); v.extend(res.split("\n"));
            self.location.line += (v.len()) - 1;
            if v.len() == 1 {
                self.location.column += v[0].len();
            } else {
                self.location.column = v[v.len() -1].len();
            }
            v.concat()
        }))
    );

    method!(pub space_opt<Parser, &str, Option<String> >, mut self, opt!(call_m!(self.space)) );

    // xkcd://1638
    method!(pub character<Parser, &str, Vec<&str> >, self, re_capture!(r#"^('(\\\\|\\"|\\'|[ -\x7F]|(\\x[0-9A-Fa-f][0-9A-Fa-f]))')"#) );


    method!(dec<Parser, &str, &str>, self, re_find!(r"^[1-9][0-9]*") );

    method!(hex<Parser, &str, Vec<&str> >, self, re_capture!(r"^0x([0-9A-Fa-f]*)") );

    method!(oct<Parser, &str, &str>, self, re_find!(r"^0[0-7]*") );

    method!(pub integer<Parser, &str, Node<Expression> >, mut self,
        do_parse!(
            start: call_m!(self.getLocation) >>
            t: alt!(
                terminated!(do_parse!(
                decimal: call_m!(self.dec) >>
                value: expr_opt!( decimal.parse::<i64>().ok()) >>
                ({self.location.column += decimal.len(); ast::Expression::Int(value)})
                ), call_m!(self.space_opt))
            |   terminated!(do_parse!(
                hexa: call_m!(self.hex) >>
                value: expr_opt!( i64::from_str_radix(hexa[1], 16).ok() ) >>
                ({self.location.column += hexa[0].len(); ast::Expression::Int(value)})
                ), call_m!(self.space_opt))
            |       terminated!(do_parse!(
                oct: call_m!(self.oct) >>
                value: expr_opt!(i64::from_str_radix(oct, 8).ok() ) >>
                ({self.location.column += oct.len(); ast::Expression::Int(value)})
                ), call_m!(self.space_opt))
            |   terminated!(do_parse!(tag!("0") >> ({self.location.column += 1; ast::Expression::Int(0)})), call_m!(self.space_opt))
            |   terminated!(do_parse!(ch: call_m!(self.character) >> res: expr_opt!(convert_char(ch[2])) >> take_s!(1)>> ({self.location.column += ch[0].len() + 1; ast::Expression::Int(res)})), call_m!(self.space_opt)) // It seems tha \x7f confuses the regexp.

            ) >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: t})

        )
    );



    // Miscellaneous tokens : Comma, parenthesis, braces, ...
    method!(pub comma       <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!(",") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub semicolon   <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!(";") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub brace_open  <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("{") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub brace_close <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("}") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub parens_open <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("(") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub parens_close<Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!(")") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));

    // Operators
    method!(pub op_plus     <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("+") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub op_minus    <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("-") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub op_star     <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("*") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub op_div      <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("/") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub op_not      <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("!") >> ({self.location.column += 1; s})), call_m!(self.space_opt)));

    method!(pub op_deref    <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("->") >> ({self.location.column += 2; s})), call_m!(self.space_opt)));
    method!(pub op_and      <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("&&") >> ({self.location.column += 2; s})), call_m!(self.space_opt)));
    method!(pub op_or       <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("||") >> ({self.location.column += 2; s})), call_m!(self.space_opt)));

    method!(pub op_simple_eq    <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("=" ) >> ({self.location.column += 2; s})), call_m!(self.space_opt)));
    method!(pub op_double_eq    <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("==") >> ({self.location.column += 2; s})), call_m!(self.space_opt)));
    method!(pub op_not_eq       <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("!=") >> ({self.location.column += 2; s})), call_m!(self.space_opt)));

    method!(pub op_gt   <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!(">" ) >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub op_lt   <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("<" ) >> ({self.location.column += 1; s})), call_m!(self.space_opt)));
    method!(pub op_ge   <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!(">=") >> ({self.location.column += 2; s})), call_m!(self.space_opt)));
    method!(pub op_le   <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("<=") >> ({self.location.column += 2; s})), call_m!(self.space_opt)));


    // Operator groups.
    method!(pub unary_op<Parser, &str, Node<UnaryOp>>, mut self, alt!(
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_not)   >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: UnaryOp::Not}))
        | do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_minus) >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: UnaryOp::Minus}))
    ));

    method!(pub binary_op_mul<Parser, &str, Node<BinaryOp> >, mut self, alt!(
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_star)   >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Mult}))
        | do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_div)    >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Div}))
    ));

    method!(pub binary_op_arith<Parser, &str, Node<BinaryOp> >, mut self, alt!(
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_plus)   >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Plus}))
        | do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_minus)  >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Minus}))
    ));

    method!(pub binary_op_compare<Parser, &str, Node<BinaryOp> >, mut self, alt!(
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_le) >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::LowerEq}))
        | do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_ge) >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::GreaterEq}))
        | do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_lt) >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Lower}))
        | do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_gt) >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Greater}))
    ));

    method!(pub binary_op_eq<Parser, &str, Node<BinaryOp> >, mut self, alt!(
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_double_eq)  >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Equal}))
        | do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_not_eq)     >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::NotEqual}))
    ));

    method!(pub binary_op_and<Parser, &str, Node<BinaryOp> >, mut self,
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_and)   >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::And}))
    );

    method!(pub binary_op_or<Parser, &str, Node<BinaryOp> >, mut self,
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_or)   >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Or}))
    );

    method!(pub binary_op_affect<Parser, &str, Node<BinaryOp> >, mut self,
          do_parse!(
            start: call_m!(self.getLocation) >>
            call_m!(self.op_simple_eq)   >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: BinaryOp::Affect}))
    );

    // Keywords : Do not forget to update check_keyword below.
    method!(pub kwd_int     <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("int"    ) >> ({self.location.column += 3; s})), call_m!(self.space_opt)) );
    method!(pub kwd_struct  <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("struct" ) >> ({self.location.column += 6; s})), call_m!(self.space_opt)) );
    method!(pub kwd_sizeof  <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("sizeof" ) >> ({self.location.column += 6; s})), call_m!(self.space_opt)) );
    method!(pub kwd_if      <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("if"     ) >> ({self.location.column += 2; s})), call_m!(self.space_opt)) );
    method!(pub kwd_else    <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("else"   ) >> ({self.location.column += 4; s})), call_m!(self.space_opt)) );
    method!(pub kwd_while   <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("while"  ) >> ({self.location.column += 5; s})), call_m!(self.space_opt)) );
    method!(pub kwd_return  <Parser, &str, &str>, mut self, terminated!(do_parse!(s: tag_s!("return" ) >> ({self.location.column += 6; s})), call_m!(self.space_opt)) );

    method!(pub identifier <Parser, &str, Node<Ident> >, mut self,
        terminated!(
        do_parse!(
            start: call_m!(self.getLocation) >>
            as_str: re_find!(r"^[a-zA-Z_][a-zA-Z_1-9]*") >>
            res: expr_opt!(check_keyword(as_str)) >>
            stop: call_m!(self.getLocation) >>
            (Node{start: start, stop: stop, t: {self.location.column += res.len(); res} } )

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
