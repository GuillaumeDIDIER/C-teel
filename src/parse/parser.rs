
use parse::ast;
//use std::vec;


use parse::lexer::*;

pub struct Parser {
    pub line: i64,
    pub column: i64,
}
impl Parser {

    pub fn new() -> Parser {

        Parser{line: 0, column:0 }
      }

    method!(pub file<Parser, &str,  Vec<ast::Declaration> >, mut self,
    sp!(terminated!(
            many0!( call_m!(self.decl) )
            , eof!()
    )));


    // Declarations
    // Variable declaration
    method!(pub decl_vars<Parser, &str, ast::DeclVar>, mut self, alt_complete!(
        sp!(do_parse!(kwd_int >> ids: sp!(separated_nonempty_list!(tag_s!(","), sp!(identifier))) >> tag_s!(";") >> (ast::DeclVar::Int(ids))))
        | sp!(do_parse!(
            sp!(kwd_struct) >> id: sp!(identifier) >>
            ids: sp!(separated_nonempty_list!(sp!(tag_s!(",")), sp!(preceded!(sp!(tag_s!("*")), sp!(identifier))))) >> sp!(tag_s!(";")) >>
            (ast::DeclVar::Struct(id, ids))))
    ));

    // Type
    method!(pub decl_typ<Parser, &str, ast::DeclType>, mut self, sp!(do_parse!(
        kwd_struct >> id: identifier >> tag_s!("{") >>
        vars: many0!(call_m!(self.decl_vars)) >>
        tag_s!("}") >> tag_s!(";") >>
        ((id, vars))
    )));


    // Function
    //   Parameter (aux)
    method!(pub param<Parser, &str, ast::Param>, mut self, alt_complete!(
        sp!(do_parse!(
            kwd_int >> id: identifier >> (ast::Param::Int(id))
        ))
        |
        sp!(do_parse!(
            kwd_struct >> typ: identifier >> tag_s!("*") >> id: identifier >> (ast::Param::Struct(typ, id))
        ))
    ));


    method!(pub decl_fct_aux<Parser, &str, (ast::Ident, Vec<ast::Param>, ast::Bloc) >, mut self, sp!(do_parse!(
        id: identifier >>
        tag_s!("(") >>
        params: separated_list!(tag_s!(","), call_m!(self.param)) >>
        tag_s!(")") >>
        blk: call_m!(self.bloc) >>
        ((id, params, blk)))
    ));

    method!(pub decl_fct<Parser, &str, ast::DeclFunc>, mut self, alt_complete!(
        sp!(do_parse!(
            kwd_int >> funct: call_m!(self.decl_fct_aux) >> (ast::DeclFunc::Int(funct.0, funct.1, funct.2))
        ))
        |
        sp!(do_parse!(
            kwd_struct >> id: identifier >> tag_s!("*") >> funct: call_m!(self.decl_fct_aux) >> (ast::DeclFunc::Struct(id, funct.0, funct.1, funct.2))
        ))
    ));


    method!(pub decl<Parser, &str, ast::Declaration>, mut self, alt_complete!(
        sp!(do_parse!(v: call_m!(self.decl_vars) >> (ast::Declaration::Var(v))))
        | sp!(do_parse!(f: call_m!(self.decl_fct) >> (ast::Declaration::Func(f))))
        | sp!(do_parse!(t: call_m!(self.decl_typ) >> (ast::Declaration::Type(t))))
    ));

    // Bloc
    method!(pub bloc<Parser, &str, ast::Bloc>, mut self, sp!(do_parse!(
        sp!(tag_s!("{")) >>
        vars: sp!(many0!(call_m!(self.decl_vars))) >>
        stmts: sp!(many0!(call_m!(self.statement))) >>
        sp!(tag_s!("}")) >>
        ((vars,stmts))
    )));

    // Statements
    method!(pub statement<Parser, &str, ast::Statement>, mut self, alt_complete!(
        sp!(do_parse!(tag_s!(";") >> (ast::Statement::Noop)))
        | sp!(do_parse!(e: expr >> sp!(tag_s!(";")) >> (ast::Statement::Expr(e))))
        | sp!(do_parse!(
            kwd_if >> sp!(tag_s!("(")) >> cond: expr>> sp!(tag_s!(")")) >>
            if_s: call_m!(self.statement) >>
            kwd_else >>
            else_s: call_m!(self.statement) >>
            (ast::Statement::IfElse(cond, Box::new(if_s), Box::new(else_s)))))
        | sp!(do_parse!(
            kwd_if >> sp!(tag_s!("(")) >> cond: expr>> sp!(tag_s!(")")) >>
            if_s: call_m!(self.statement) >>
            (ast::Statement::If(cond, Box::new(if_s)))))
        | sp!(do_parse!(
            kwd_while >> tag_s!("(") >> cond: expr>> tag_s!(")") >>
            while_s: call_m!(self.statement) >>
            (ast::Statement::While(cond, Box::new(while_s)))))
        | sp!(do_parse!(
            kwd_return >> val: expr >> tag_s!(";") >>
            (ast::Statement::Return(val))))
        | sp!(do_parse!(blk: call_m!(self.bloc) >> (ast::Statement::Bloc(blk))))
    ));

}










// Expressions

named!(pub parens<&str, ast::Expression>, sp!(do_parse!(
    tag_s!("(") >>
    sub_expr: expr >>
    tag_s!(")") >>
    (ast::Expression::Parens(Box::new(sub_expr)))
)));

named!(pub expr<&str, ast::Expression>, sp!(affect_expr));

named!(sizeof_struct<&str, ast::Expression>, sp!(do_parse!(
    kwd_sizeof >>
    tag_s!("(") >>
    kwd_struct >>
    id: identifier >>
    tag_s!(")") >>
    (ast::Expression::Sizeof(id))
)));

named!(pub call<&str, ast::Expression>, sp!(do_parse!(
    id: identifier >>
    tag_s!("(") >>
    params: sp!(separated_list!(sp!(tag_s!(",")), expr)) >>
    tag_s!(")") >>
    (ast::Expression::Call(id, params))
)));

named!(atom<&str, ast::Expression> , alt_complete!(

      call
    | parens
    | integer
    | sizeof_struct
    | do_parse!(id: identifier >> (ast::Expression::Ident(id)))
));

pub fn build_deref_tree(first: ast::Expression , v: Vec<ast::Ident>) -> ast::Expression{
    let mut vm = v.clone();
    build_deref_tree_aux(first, &mut vm)
}

pub fn build_deref_tree_aux(first: ast::Expression , v: &mut Vec<ast::Ident>) -> ast::Expression {
    if let Some(i) = v.pop() {
        ast::Expression::MembDeref(Box::new(build_deref_tree_aux(first, v)), i)
    } else {
        first
    }
}

named!(pub deref<&str, ast::Expression>, sp!(do_parse!(
    first: atom >>
    v: many0!(
        do_parse!(
            tag_s!("->") >>
            i: identifier >>
            (i))
        ) >>
    (build_deref_tree(first, v))
    )
));



named!(factor<&str, ast::Expression>, alt!(sp!(do_parse!(
    op: unary_op >>
    un: factor >>
    (ast::Expression::Unary(op, Box::new(un)))
))
| deref
));

fn build_binop_left_assoc_tree(first: ast::Expression , v: Vec<(ast::BinaryOp,ast::Expression)>) -> ast::Expression{
    let mut vm = v.clone();
    build_binop_left_assoc_tree_aux(first, &mut vm)
}

fn build_binop_left_assoc_tree_aux(first: ast::Expression , v: &mut Vec<(ast::BinaryOp,ast::Expression)>) -> ast::Expression {
    if let Some((op, second)) = v.pop() {
        ast::Expression::Binary(Box::new(build_binop_left_assoc_tree_aux(first, v)), op, Box::new(second))
    } else {
        first
    }
}

named!(term<&str, ast::Expression>, sp!(do_parse!(
    first: factor >>
    v: many0!(
        do_parse!(
            op: binary_op_mul >>
            f: factor >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
));

named!(arith<&str, ast::Expression>, sp!(do_parse!(
    first: term >>
    v: many0!(
        do_parse!(
            op: binary_op_arith >>
            f: term >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
));

named!(compare<&str, ast::Expression>, sp!(do_parse!(
    first: arith >>
    v: many0!(
        do_parse!(
            op: binary_op_compare >>
            f: arith >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
));

named!(eq<&str, ast::Expression>, sp!(do_parse!(
    first: compare >>
    v: many0!(
        do_parse!(
            op: binary_op_eq >>
            f: compare >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
));

named!(and_expr<&str, ast::Expression>, sp!(do_parse!(
    first: eq >>
    v: many0!(
        do_parse!(
            tag_s!("&&") >>
            f: eq >>
            ((ast::BinaryOp::And,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
));

named!(pub or_expr<&str, ast::Expression>, sp!(do_parse!(
    first: and_expr >>
    v: many0!(
        do_parse!(
            tag_s!("||") >>
            f: and_expr >>
            ((ast::BinaryOp::Or,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    ))
);

named!(pub affect_expr<&str, ast::Expression>,alt_complete!(
    sp!(do_parse!(
        lhs: or_expr >>
        rhs: sp!(preceded!(sp!(tag_s!("=")), expr))  >>
        (ast::Expression::Binary(Box::new(lhs), ast::BinaryOp::Affect, Box::new(rhs)))
    ))
    | or_expr
));
