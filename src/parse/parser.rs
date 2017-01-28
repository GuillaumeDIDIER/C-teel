
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
    do_parse!(
            decls: many0!( call_m!(self.decl) ) >>
            call_m!(self.space_opt) >>
            eof!() >>
            (decls)
    ));


    // Declarations
    // Variable declaration
    method!(pub decl_vars<Parser, &str, ast::DeclVar>, mut self, alt_complete!(
        do_parse!(call_m!(self.kwd_int) >> ids: separated_nonempty_list!(preceded!(call_m!(self.space_opt), tag_s!(",")), call_m!(self.identifier)) >> preceded!(call_m!(self.space_opt), tag_s!(";")) >> (ast::DeclVar::Int(ids)))
        | do_parse!(
            call_m!(self.kwd_struct) >> id: call_m!(self.identifier) >>
            ids: separated_nonempty_list!(preceded!(call_m!(self.space_opt), tag_s!(",")), preceded!(preceded!(call_m!(self.space_opt), tag_s!("*")), call_m!(self.identifier))) >> preceded!(call_m!(self.space_opt), tag_s!(";")) >>
            (ast::DeclVar::Struct(id, ids)))
    ));

    // Type
    method!(pub decl_typ<Parser, &str, ast::DeclType>, mut self, do_parse!(
        call_m!(self.kwd_struct) >> id: call_m!(self.identifier) >> preceded!(call_m!(self.space_opt), tag_s!("{")) >>
        vars: many0!(call_m!(self.decl_vars)) >>
        preceded!(call_m!(self.space_opt), tag_s!("}")) >> preceded!(call_m!(self.space_opt), tag_s!(";")) >>
        ((id, vars))
    ));


    // Function
    //   Parameter (aux)
    method!(pub param<Parser, &str, ast::Param>, mut self, alt_complete!(
        do_parse!(
            call_m!(self.kwd_int) >> id: call_m!(self.identifier) >> (ast::Param::Int(id))
        )
        |
        do_parse!(
            call_m!(self.kwd_struct) >> typ: call_m!(self.identifier) >> preceded!(call_m!(self.space_opt), tag_s!("*")) >> id: call_m!(self.identifier) >> (ast::Param::Struct(typ, id))
        )
    ));


    method!(pub decl_fct_aux<Parser, &str, (ast::Ident, Vec<ast::Param>, ast::Bloc) >, mut self, do_parse!(
        id: call_m!(self.identifier) >>
        preceded!(call_m!(self.space_opt), tag_s!("(")) >>
        params: separated_list!(preceded!(call_m!(self.space_opt), tag_s!(",")), call_m!(self.param)) >>
        preceded!(call_m!(self.space_opt), tag_s!(")")) >>
        blk: call_m!(self.bloc) >>
        ((id, params, blk)))
    );

    method!(pub decl_fct<Parser, &str, ast::DeclFunc>, mut self, alt_complete!(
        do_parse!(
            call_m!(self.kwd_int) >> funct: call_m!(self.decl_fct_aux) >> (ast::DeclFunc::Int(funct.0, funct.1, funct.2))
        )
        |
        do_parse!(
            call_m!(self.kwd_struct) >> id: call_m!(self.identifier) >> preceded!(call_m!(self.space_opt), tag_s!("*")) >> funct: call_m!(self.decl_fct_aux) >> (ast::DeclFunc::Struct(id, funct.0, funct.1, funct.2))
        )
    ));


    method!(pub decl<Parser, &str, ast::Declaration>, mut self, alt_complete!(
        do_parse!(v: call_m!(self.decl_vars) >> (ast::Declaration::Var(v)))
        | do_parse!(f: call_m!(self.decl_fct) >> (ast::Declaration::Func(f)))
        | do_parse!(t: call_m!(self.decl_typ) >> (ast::Declaration::Type(t)))
    ));

    // Bloc
    method!(pub bloc<Parser, &str, ast::Bloc>, mut self, do_parse!(
        preceded!(call_m!(self.space_opt), tag_s!("{")) >>
        vars: many0!(call_m!(self.decl_vars)) >>
        stmts: many0!(call_m!(self.statement)) >>
        preceded!(call_m!(self.space_opt), tag_s!("}")) >>
        ((vars,stmts))
    ));

    // Statements
    method!(pub statement<Parser, &str, ast::Statement>, mut self, alt_complete!(
        do_parse!(preceded!(call_m!(self.space_opt), tag_s!(";")) >> (ast::Statement::Noop))
        | do_parse!(e: call_m!(self.expr) >> preceded!(call_m!(self.space_opt), tag_s!(";")) >> (ast::Statement::Expr(e)))
        | do_parse!(
            call_m!(self.kwd_if) >> preceded!(call_m!(self.space_opt), tag_s!("(")) >> cond: call_m!(self.expr)>> preceded!(call_m!(self.space_opt), tag_s!(")")) >>
            if_s: call_m!(self.statement) >>
            call_m!(self.kwd_else) >>
            else_s: call_m!(self.statement) >>
            (ast::Statement::IfElse(cond, Box::new(if_s), Box::new(else_s))))
        | do_parse!(
            call_m!(self.kwd_if) >> preceded!(call_m!(self.space_opt), tag_s!("(")) >> cond: call_m!(self.expr)>> preceded!(call_m!(self.space_opt), tag_s!(")")) >>
            if_s: call_m!(self.statement) >>
            (ast::Statement::If(cond, Box::new(if_s))))
        | do_parse!(
            call_m!(self.kwd_while) >> preceded!(call_m!(self.space_opt), tag_s!("(")) >> cond: call_m!(self.expr)>> preceded!(call_m!(self.space_opt), tag_s!(")")) >>
            while_s: call_m!(self.statement) >>
            (ast::Statement::While(cond, Box::new(while_s))))
        | do_parse!(
            call_m!(self.kwd_return) >> val: call_m!(self.expr) >> preceded!(call_m!(self.space_opt), tag_s!(";")) >>
            (ast::Statement::Return(val)))
        | do_parse!(blk: call_m!(self.bloc) >> (ast::Statement::Bloc(blk)))
    ));

    // Expressions



    //method!(pub expr<Parser, &str, ast::Expression>, mut self, sp!(affect_expr));
    method!(pub expr<Parser, &str, ast::Expression>, mut self, alt_complete!(
        do_parse!(
            lhs: call_m!(self.or_expr) >>
            rhs: preceded!(preceded!(call_m!(self.space_opt), tag_s!("=")), call_m!(self.expr))  >>
            (ast::Expression::Binary(Box::new(lhs), ast::BinaryOp::Affect, Box::new(rhs)))
        )
        | call_m!(self.or_expr)
    ));

    method!(pub or_expr<Parser, &str, ast::Expression>, mut self, do_parse!(
        first: call_m!(self.and_expr) >>
        v: many0!(
            do_parse!(
                preceded!(call_m!(self.space_opt), tag_s!("||")) >>
                f: call_m!(self.and_expr)  >>
                ((ast::BinaryOp::Or,f)))
            ) >>
            (build_binop_left_assoc_tree(first, v))
    ));

    method!(and_expr<Parser, &str, ast::Expression>, mut self, do_parse!(
        first: call_m!(self.eq) >>
        v: many0!(
            do_parse!(
                preceded!(call_m!(self.space_opt), tag_s!("&&")) >>
                f: call_m!(self.eq) >>
                ((ast::BinaryOp::And,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(eq<Parser, &str, ast::Expression>, mut self, do_parse!(
        first: call_m!(self.compare) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_eq) >>
                f: call_m!(self.compare) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(compare<Parser, &str, ast::Expression>, mut self, do_parse!(
        first: call_m!(self.arith) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_compare) >>
                f: call_m!(self.arith) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(arith<Parser, &str, ast::Expression>, mut self, do_parse!(
        first: call_m!(self.term) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_arith) >>
                f: call_m!(self.term) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(term<Parser, &str, ast::Expression>, mut self, do_parse!(
        first: call_m!(self.factor) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_mul) >>
                f: call_m!(self.factor) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(factor<Parser, &str, ast::Expression>, mut self, alt!(do_parse!(
        op: call_m!(self.unary_op) >>
        un: call_m!(self.factor) >>
        (ast::Expression::Unary(op, Box::new(un)))
    )
    | call_m!(self.deref)
    ));

    method!(deref<Parser, &str, ast::Expression>, mut self, do_parse!(
        first: call_m!(self.atom) >>
        v: many0!(
            do_parse!(
                preceded!(call_m!(self.space_opt), tag_s!("->")) >>
                i: call_m!(self.identifier) >>
                (i))
            ) >>
        (build_deref_tree(first, v))
    ));

    method!(atom<Parser, &str, ast::Expression>, mut self, alt_complete!(
          call_m!(self.call)
        | call_m!(self.parens)
        | call_m!(self.integer)
        | call_m!(self.sizeof_struct)
        | do_parse!(id: call_m!(self.identifier) >> (ast::Expression::Ident(id)))
    ));

    method!(call<Parser, &str, ast::Expression>, mut self, do_parse!(
        id: call_m!(self.identifier) >>
        preceded!(call_m!(self.space_opt), tag_s!("(")) >>
        params: separated_list!(preceded!(call_m!(self.space_opt), tag_s!(",")), call_m!(self.expr)) >>
        preceded!(call_m!(self.space_opt), tag_s!(")")) >>
        (ast::Expression::Call(id, params))
    ));

    method!(parens<Parser, &str, ast::Expression>, mut self, do_parse!(
        preceded!(call_m!(self.space_opt), tag_s!("(")) >>
        sub_expr: call_m!(self.expr) >>
        preceded!(call_m!(self.space_opt), tag_s!(")")) >>
        (ast::Expression::Parens(Box::new(sub_expr)))
    ));

    method!(sizeof_struct<Parser, &str, ast::Expression>, mut self, do_parse!(
        call_m!(self.kwd_sizeof) >>
        preceded!(call_m!(self.space_opt), tag_s!("(")) >>
        call_m!(self.kwd_struct) >>
        id: call_m!(self.identifier) >>
        preceded!(call_m!(self.space_opt), tag_s!(")")) >>
        (ast::Expression::Sizeof(id))
    ));

}

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
