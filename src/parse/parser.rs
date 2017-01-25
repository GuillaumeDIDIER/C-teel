
use parse::ast;
//use std::vec;


use parse::lexer::*;


named!(pub file<&str, Vec<ast::Declaration> >,
    terminated!(
        many0!(decl)
        , eof!()
));

// Declarations
// Variable declaration
named!(pub decl_vars<&str, ast::DeclVar>, alt_complete!(
    do_parse!(kwd_int >> ids: separated_nonempty_list!(tag_s!(","), identifier) >> tag_s!(";") >> (ast::DeclVar::Int(ids)))
    | do_parse!(
        kwd_struct >> id: identifier >>
        ids: separated_nonempty_list!(tag_s!(","), preceded!(tag_s!("*"), identifier)) >> tag_s!(";") >>
        (ast::DeclVar::Struct(id, ids)))
));

// Type
named!(pub decl_typ<&str, ast::DeclType>, do_parse!(
    kwd_struct >> id: identifier >> tag_s!("{") >>
    vars: many0!(decl_vars) >>
    tag_s!("}") >> tag_s!(";") >>
    ((id, vars))
));

// Function
//   Parameter (aux)
named!(param<&str, ast::Param>, alt_complete!(
    do_parse!(
        kwd_int >> id: identifier >> (ast::Param::Int(id))
    )
    |
    do_parse!(
        kwd_struct >> typ: identifier >> tag_s!("*") >> id: identifier >> (ast::Param::Struct(typ, id))
    )
));


named!(decl_fct_aux<&str, (ast::Ident, Vec<ast::Param>, ast::Bloc) >, do_parse!(
    id: identifier >>
    tag_s!("(") >>
    params: separated_list!(tag_s!(","), param) >>
    tag_s!(")") >>
    blk: bloc >>
    ((id, params, blk))
));

named!(pub decl_fct<&str, ast::DeclFunc>, alt_complete!(
    do_parse!(
        kwd_int >> funct: decl_fct_aux >> (ast::DeclFunc::Int(funct.0, funct.1, funct.2))
    )
    |
    do_parse!(
        kwd_struct >> id: identifier >> tag_s!("*") >> funct: decl_fct_aux >> (ast::DeclFunc::Struct(id, funct.0, funct.1, funct.2))
    )
));

// All decl
named!(decl<&str, ast::Declaration>, alt_complete!(
    do_parse!(v: decl_vars >> (ast::Declaration::Var(v)))
    | do_parse!(f: decl_fct >> (ast::Declaration::Func(f)))
    | do_parse!(t: decl_typ >> (ast::Declaration::Type(t)))
));


// Bloc
named!(pub bloc<&str, ast::Bloc>, do_parse!(
    tag_s!("{") >>
    vars: many0!(decl_vars) >>
    stmts: many0!(statement) >>
    ((vars,stmts))
));

// Statements
named!(pub statement<&str, ast::Statement>, sp!(alt_complete!(
    do_parse!(tag_s!(";") >> (ast::Statement::Noop))
    | do_parse!(e: expr >> tag_s!(";") >> (ast::Statement::Expr(e)))
    | do_parse!(
        kwd_if >> tag_s!("(") >> cond: expr>> tag_s!(")") >>
        if_s: statement >>
        kwd_else >>
        else_s: statement >>
        (ast::Statement::IfElse(cond, Box::new(if_s), Box::new(else_s))))
    | do_parse!(
        kwd_if >> tag_s!("(") >> cond: expr>> tag_s!(")") >>
        if_s: statement >>
        (ast::Statement::If(cond, Box::new(if_s))))
    | do_parse!(
        kwd_while >> tag_s!("(") >> cond: expr>> tag_s!(")") >>
        while_s: statement >>
        (ast::Statement::While(cond, Box::new(while_s))))
    | do_parse!(
        kwd_return >> val: expr >> tag_s!(";") >>
        (ast::Statement::Return(val)))
    | do_parse!(blk: bloc >> (ast::Statement::Bloc(blk)))
)));

// Expressions

named!(pub parens<&str, ast::Expression>, do_parse!(
    tag_s!("(") >>
    sub_expr: expr >>
    tag_s!(")") >>
    (ast::Expression::Parens(Box::new(sub_expr)))
));

named!(pub expr<&str, ast::Expression>, alt_complete!(affect_expr));

named!(sizeof_struct<&str, ast::Expression>, do_parse!(
    kwd_sizeof >>
    tag_s!("(") >>
    kwd_struct >>
    id: identifier >>
    tag_s!(")") >>
    (ast::Expression::Sizeof(id))
));

named!(call<&str, ast::Expression>, do_parse!(
    id: identifier >>
    tag_s!("(") >>
    params: separated_list!(tag_s!(","), expr) >>
    (ast::Expression::Call(id, params))
));

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

named!(pub deref<&str, ast::Expression>, do_parse!(
    first: atom >>
    v: many0!(
        do_parse!(
            tag_s!("->") >>
            i: identifier >>
            (i))
        ) >>
    (build_deref_tree(first, v))
    )
);



named!(factor<&str, ast::Expression>, alt!(do_parse!(
    op: unary_op >>
    un: factor >>
    (ast::Expression::Unary(op, Box::new(un)))
)
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

named!(term<&str, ast::Expression>, do_parse!(
    first: factor >>
    v: many0!(
        do_parse!(
            op: binary_op_mul >>
            f: factor >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
);

named!(arith<&str, ast::Expression>, do_parse!(
    first: term >>
    v: many0!(
        do_parse!(
            op: binary_op_arith >>
            f: term >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
);

named!(compare<&str, ast::Expression>, do_parse!(
    first: arith >>
    v: many0!(
        do_parse!(
            op: binary_op_compare >>
            f: arith >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
);

named!(eq<&str, ast::Expression>, do_parse!(
    first: compare >>
    v: many0!(
        do_parse!(
            op: binary_op_eq >>
            f: compare >>
            ((op,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
);

named!(and_expr<&str, ast::Expression>, do_parse!(
    first: eq >>
    v: many0!(
        do_parse!(
            tag_s!("&&") >>
            f: eq >>
            ((ast::BinaryOp::And,f)))
        ) >>
    (build_binop_left_assoc_tree(first, v))
    )
);

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
    do_parse!(
        lhs: or_expr >>
        rhs: preceded!(tag_s!("="), or_expr)  >>
        (ast::Expression::Binary(Box::new(lhs), ast::BinaryOp::Affect, Box::new(rhs)))
    )
    | or_expr
));
