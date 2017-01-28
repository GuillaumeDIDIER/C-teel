
//use std::vec;
//use std::boxed;

pub type Ident =  String;

#[derive(Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize
}

#[derive(Clone)]
pub enum UnaryOp{
    Not,
    Minus,
}

#[derive(Clone)]
pub enum BinaryOp{
    Affect,
    Equal,
    NotEqual,
    Lower,
    LowerEq,
    Greater,
    GreaterEq,
    Plus,
    Minus,
    Mult,
    Div,
    And,
    Or,
}

#[derive(Clone)]
pub enum Param {
    Int(Ident),
    Struct(Ident, Ident),
}

#[derive(Clone)]
pub enum Expression {
    Int(i64),
    Ident(Ident),
    MembDeref(Box<Expression>, Ident),
    Call(Ident, Vec<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Sizeof(Ident),
    Parens(Box<Expression>),
}

pub type DeclType = (Ident, Vec<DeclVar>);

#[derive(Clone)]
pub enum DeclVar {
    Int(Vec<Ident>),
    Struct(Ident, Vec<Ident>),
}

#[derive(Clone)]
pub enum DeclFuncType {
    Int(Ident, Vec<Param>, Bloc),
    Struct(Ident, Ident, Vec<Param>, Bloc),
}

#[derive(Clone)]
pub struct DeclFunc {
    pub start: Location,
    pub stop: Location,
    pub t: DeclFuncType,
}

#[derive(Clone)]
pub enum DeclarationType {
    Var(DeclVar),
    Type(DeclType),
    Func(DeclFunc),
}

#[derive(Clone)]
pub struct Declaration {
    pub start: Location,
    pub stop:  Location,
    pub t: DeclarationType,
}

pub type Bloc = (Vec<DeclVar>,Vec<Statement>);

#[derive(Clone)]
pub enum Statement {
    Expr(Expression),
    If(Expression, Box<Statement>),
    IfElse(Expression, Box<Statement>, Box<Statement>),
    While(Expression, Box<Statement>),
    Return(Expression),
    Bloc(Bloc),
    Noop,
}
