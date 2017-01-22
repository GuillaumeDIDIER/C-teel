
use std::vec;
use std::boxed;

pub type Ident =  String;

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

enum Param {
    Int(Ident),
    Struct(Ident),
}

enum Expression {
    int(i32),
    Ident(Ident),
    MembDeref(Box<Expression>, Ident),
    Call(Ident, Vec<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Sizeof(Ident),
    Parens(Vec<Expression>),
}

type DeclType = (Ident, Vec<DeclVar>);

enum DeclVar {
    Int(Vec<Ident>),
    Struct(Ident, Vec<Ident>),
}

enum DeclFunc {
    Int(Ident, Vec<Param>, Bloc)
}

enum Declaration {
    Var(DeclVar),
    Type(DeclType),
    Func(DeclFunc),
}

type Bloc = (Vec<DeclVar>,Vec<Statement>);

enum Statement {
    Expr(Expression),
    If(Expression, Box<Statement>),
    IfElse(Expression, Box<Statement>, Box<Statement>),
    While(Expression, Box<Statement>),
    Return(Expression),
    Bloc(Bloc),
}
