
use parse::ast::*;
use nom;


pub struct Parser {
    pub location: Location,
}
impl Parser {
    pub fn new() -> Parser {

        Parser {
            location: Location {
                line: 0,
                column: 0,
            },
        }
    }

    pub fn get_location<'a>(self, input: &'a str) -> (Parser, nom::IResult<&'a str, Location>) {
        let l = Location::clone(&self.location);
        (self, nom::IResult::Done(input, l))
    }

    method!(pub file<Parser, &str,  Vec< Node<Declaration> > >, mut self,
    do_parse!(
            call_m!(self.space_opt) >>
            decls: many0!( call_m!(self.decl) ) >>
//            call_m!(self.space_opt) >>
            eof!() >>
            (decls)
    ));


    // Declarations
    // Variable declaration
    method!(pub decl_vars<Parser, &str, Node<DeclVar> >, mut self, alt_complete!(
        do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_int) >>
            ids: separated_nonempty_list!(call_m!(self.comma), call_m!(self.identifier)) >>
            call_m!(self.semicolon) >>
            stop: call_m!(self.get_location) >>
            (Node{start: start, stop: stop, t: DeclVar::Int(ids)}))
        | do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_struct) >>
            id: call_m!(self.identifier) >>
            ids: separated_nonempty_list!(call_m!(self.comma), preceded!(call_m!(self.op_star), call_m!(self.identifier))) >>
            call_m!(self.semicolon) >>
            stop: call_m!(self.get_location) >>
            (Node{start: start, stop: stop, t: DeclVar::Struct(id, ids)}))
    ));

    // Type
    method!(pub decl_typ<Parser, &str, Node<DeclType>>, mut self, do_parse!(
        start: call_m!(self.get_location) >>
        call_m!(self.kwd_struct) >>
        id: call_m!(self.identifier) >>
        call_m!(self.brace_open) >>
        vars: many0!(call_m!(self.decl_vars)) >>
        call_m!(self.brace_close) >>
        call_m!(self.semicolon) >>
        stop: call_m!(self.get_location) >>
        (Node{start: start, stop: stop, t: (id, vars)})
    ));


    // Function
    //   Parameter (aux)
    method!(pub param<Parser, &str, Node<Param>>, mut self, alt_complete!(
        do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_int) >>
            id: call_m!(self.identifier) >>
            (Node{start: start, stop: id.stop.clone(), t: Param::Int(id)})
        )
        |
        do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_struct) >>
            typ: call_m!(self.identifier) >>
            call_m!(self.op_star) >>
            id: call_m!(self.identifier) >>
            (Node{start: start, stop: id.stop.clone(), t: Param::Struct(typ, id)})
        )
    ));


    method!(pub decl_fct_aux<Parser, &str, (Node<Ident>, Vec<Node<Param> >, Node<Bloc>) >, mut self, do_parse!(
        id: call_m!(self.identifier) >>
        call_m!(self.parens_open) >>
        params: separated_list!(call_m!(self.comma), call_m!(self.param)) >>
        call_m!(self.parens_close) >>
        blk: call_m!(self.bloc) >>
        ((id, params, blk)))
    );

    method!(pub decl_fct<Parser, &str, Node<DeclFunc>>, mut self, do_parse!(
        start: call_m!(self.get_location) >>
        t: alt_complete!(
            do_parse!(
                call_m!(self.kwd_int) >> funct: call_m!(self.decl_fct_aux) >>
                (DeclFunc::Int(funct.0, funct.1, funct.2))
            )
            |
            do_parse!(
                call_m!(self.kwd_struct) >> id: call_m!(self.identifier) >> call_m!(self.op_star) >> funct: call_m!(self.decl_fct_aux) >>
                (DeclFunc::Struct(id, funct.0, funct.1, funct.2))
            )
        ) >>
        stop: call_m!(self.get_location) >>
        (Node{start: start, stop: stop, t: t})
    ));


    method!(pub decl<Parser, &str, Node<Declaration> >, mut self, do_parse!(
        start: call_m!(self.get_location) >>
        t: alt_complete!(
              do_parse!(v: call_m!(self.decl_vars) >> (Declaration::Var(v)))
            | do_parse!(f: call_m!(self.decl_fct) >> (Declaration::Func(f)))
            | do_parse!(t: call_m!(self.decl_typ) >> (Declaration::Type(t)))
        ) >>
        stop: call_m!(self.get_location) >>
        (Node{start: start, stop: stop, t: t})
    ));

    // Bloc
    method!(pub bloc<Parser, &str, Node<Bloc> >, mut self, do_parse!(
        start: call_m!(self.get_location) >>
        call_m!(self.brace_open) >>
        vars: many0!(call_m!(self.decl_vars)) >>
        stmts: many0!(call_m!(self.statement)) >>
        call_m!(self.brace_close) >>
        stop: call_m!(self.get_location) >>
        (Node{start: start, stop: stop, t: (vars,stmts)})
    ));

    // Statements
    method!(pub statement<Parser, &str, Node<Statement> >, mut self, alt_complete!(
        do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.semicolon) >>
            stop: call_m!(self.get_location) >>
            (Node{start: start, stop: stop, t: Statement::Noop}))
        | do_parse!(
            e: call_m!(self.expr) >>
            call_m!(self.semicolon) >>
            stop: call_m!(self.get_location) >>
            (Node{start: e.start.clone(), stop: stop, t: Statement::Expr(e)}))
        | do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_if) >> call_m!(self.parens_open) >> cond: call_m!(self.expr)>> call_m!(self.parens_close) >>
            if_s: call_m!(self.statement) >>
            call_m!(self.kwd_else) >>
            else_s: call_m!(self.statement) >>
            (Node{start: start, stop: else_s.stop.clone(), t: Statement::IfElse(cond, Box::new(if_s), Box::new(else_s))}))
        | do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_if) >> call_m!(self.parens_open) >> cond: call_m!(self.expr)>> call_m!(self.parens_close) >>
            if_s: call_m!(self.statement) >>
            (Node{start: start, stop: if_s.stop.clone(), t: Statement::If(cond, Box::new(if_s))}))
        | do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_while) >> call_m!(self.parens_open) >> cond: call_m!(self.expr)>> call_m!(self.parens_close) >>
            while_s: call_m!(self.statement) >>
            (Node{start: start, stop: while_s.stop.clone(), t: Statement::While(cond, Box::new(while_s))}))
        | do_parse!(
            start: call_m!(self.get_location) >>
            call_m!(self.kwd_return) >> val: call_m!(self.expr) >> call_m!(self.semicolon) >>
            stop: call_m!(self.get_location) >>
            (Node{start: start, stop: stop, t: Statement::Return(val)}))
        | do_parse!(blk: call_m!(self.bloc) >> (Node{start: blk.start.clone(), stop: blk.stop.clone(), t: Statement::Bloc(blk)}))
    ));

    // Expressions



    //method!(pub expr<Parser, &str, Expression>, mut self, sp!(affect_expr));
    method!(pub expr<Parser, &str, Node<Expression> >, mut self, alt_complete!(
        do_parse!(
            lhs: call_m!(self.or_expr) >>
            op: call_m!(self.binary_op_affect) >>
            rhs: call_m!(self.expr)  >>
            (Node{start: lhs.start.clone(), stop: rhs.stop.clone(), t: Expression::Binary(Box::new(lhs), op, Box::new(rhs))})
        )
        | call_m!(self.or_expr)
    ));

    method!(pub or_expr<Parser, &str, Node< Expression> >, mut self, do_parse!(
        first: call_m!(self.and_expr) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_or) >>
                f: call_m!(self.and_expr)  >>
                ((op,f)))
            ) >>
            (build_binop_left_assoc_tree(first, v))
    ));

    method!(and_expr<Parser, &str, Node<Expression> >, mut self, do_parse!(
        first: call_m!(self.eq) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_and) >>
                f: call_m!(self.eq) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(eq<Parser, &str, Node<Expression> >, mut self, do_parse!(
        first: call_m!(self.compare) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_eq) >>
                f: call_m!(self.compare) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(compare<Parser, &str, Node< Expression> >, mut self, do_parse!(
        first: call_m!(self.arith) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_compare) >>
                f: call_m!(self.arith) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(arith<Parser, &str, Node<Expression> >, mut self, do_parse!(
        first: call_m!(self.term) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_arith) >>
                f: call_m!(self.term) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(term<Parser, &str, Node<Expression> >, mut self, do_parse!(
        first: call_m!(self.factor) >>
        v: many0!(
            do_parse!(
                op: call_m!(self.binary_op_mul) >>
                f: call_m!(self.factor) >>
                ((op,f)))
            ) >>
        (build_binop_left_assoc_tree(first, v))
    ));

    method!(factor<Parser, &str, Node<Expression> >, mut self, alt!(do_parse!(
        op: call_m!(self.unary_op) >>
        un: call_m!(self.factor) >>
        (Node{start: op.start.clone(), stop: un.stop.clone(), t: Expression::Unary(op, Box::new(un))})
    )
    | call_m!(self.deref)
    ));

    method!(deref<Parser, &str, Node<Expression> >, mut self, do_parse!(
        first: call_m!(self.atom) >>
        v: many0!(
            do_parse!(
                call_m!(self.op_deref) >>
                i: call_m!(self.identifier) >>
                (i))
            ) >>
        (build_deref_tree(first, v))
    ));

    method!(atom<Parser, &str, Node<Expression> >, mut self, alt_complete!(
            call_m!(self.call)
            | call_m!(self.parens)
            | call_m!(self.integer)
            | call_m!(self.sizeof_struct)
            | do_parse!(id: call_m!(self.identifier) >> (Node{start: id.start.clone(), stop: id.stop.clone(), t: Expression::Ident(id)}))
        )
    );

    method!(call<Parser, &str, Node<Expression>>, mut self, do_parse!(
        id: call_m!(self.identifier) >>
        call_m!(self.parens_open) >>
        params: separated_list!(call_m!(self.comma), call_m!(self.expr)) >>
        call_m!(self.parens_close) >>
        stop: call_m!(self.get_location) >>
        (Node{start: id.start.clone(), stop: stop, t: Expression::Call(id, params)})
    ));

    method!(parens<Parser, &str, Node<Expression> >, mut self, do_parse!(
        start: call_m!(self.get_location) >>
        call_m!(self.parens_open) >>
        sub_expr: call_m!(self.expr) >>
        call_m!(self.parens_close) >>
        stop: call_m!(self.get_location) >>
        (Node{start: start, stop: stop, t: Expression::Parens(Box::new(sub_expr))})
    ));

    method!(sizeof_struct<Parser, &str, Node<Expression> >, mut self, do_parse!(
        start: call_m!(self.get_location) >>
        call_m!(self.kwd_sizeof) >>
        call_m!(self.parens_open) >>
        call_m!(self.kwd_struct) >>
        id: call_m!(self.identifier) >>
        call_m!(self.parens_close) >>
        stop: call_m!(self.get_location) >>
        (Node{start: start, stop: stop, t: Expression::Sizeof(id)})
    ));
}

pub fn build_deref_tree(first: Node<Expression>, v: Vec<Node<Ident>>) -> Node<Expression> {
    let mut vm = v.clone();
    build_deref_tree_aux(first, &mut vm)
}

pub fn build_deref_tree_aux(first: Node<Expression>, v: &mut Vec<Node<Ident>>) -> Node<Expression> {
    if let Some(i) = v.pop() {
        let second = build_deref_tree_aux(first.clone(), v);

        Node {
            start: first.start,
            stop: second.stop.clone(),
            t: Expression::MembDeref(Box::new(second), i),
        }
    } else {
        first
    }
}
fn build_binop_left_assoc_tree(first: Node<Expression>,
                               v: Vec<(Node<BinaryOp>, Node<Expression>)>)
                               -> Node<Expression> {
    let mut vm = v.clone();
    build_binop_left_assoc_tree_aux(first, &mut vm)
}

fn build_binop_left_assoc_tree_aux(first: Node<Expression>,
                                   v: &mut Vec<(Node<BinaryOp>, Node<Expression>)>)
                                   -> Node<Expression> {
    if let Some((op, second)) = v.pop() {
        let first = build_binop_left_assoc_tree_aux(first, v);
        Node {
            start: first.start.clone(),
            stop: second.stop.clone(),
            t: Expression::Binary(Box::new(first), op, Box::new(second)),
        }
    } else {
        first
    }
}
