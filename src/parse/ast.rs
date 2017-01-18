
use std::vec;
use std::boxed;

pub type Ident<'a> = &'a str;

pub enum UnaryOp{
    Not,
    Minus,
}

enum BinaryOp{
    Affect,
    Equal,
    NotEqual,
    Lower,
    LowerEq,
    Greater,
    GretaerEq,
    Plus,
    Minus,
    Mult,
    Div,
    And,
    Or,
}

enum Param<'a>{
    Int(Ident<'a>),
    Struct(Ident<'a>),
}

enum Expression<'a>{
    int(i32),
    Ident(Ident<'a>),
    MembDeref(Box<Expression<'a>>, Ident<'a>),
    Call(Ident<'a>, Vec<Expression<'a>>),
    Unary(UnaryOp, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, BinaryOp, Box<Expression<'a>>),
    Sizeof(Ident<'a>),
    Parens(Vec<Expression<'a>>),
}

type DeclType<'a> = (Ident<'a>, Vec<DeclVar<'a>>);

enum DeclVar<'a>{
    Int(Vec<Ident<'a>>),
    Struct(Ident<'a>, Vec<Ident<'a>>),
}

enum DeclFunc<'a>{
    Int(Ident<'a>, Vec<Param<'a>>, Bloc<'a>)
}

enum Declaration<'a> {
    Var(DeclVar<'a>),
    Type(DeclType<'a>),
    Func(DeclFunc<'a>),
}

type Bloc<'a> = (Vec<DeclVar<'a>>,Vec<Statement<'a>>);

enum Statement<'a> {
    Expr(Expression<'a>),
    If(Expression<'a>, Box<Statement<'a>>),
    IfElse(Expression<'a>, Box<Statement<'a>>, Box<Statement<'a>>),
    While(Expression<'a>, Box<Statement<'a>>),
    Return(Expression<'a>),
    Bloc(Bloc<'a>),
}
