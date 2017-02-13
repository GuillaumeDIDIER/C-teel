use std::collections::HashMap;
pub use parse::ast::UnaryOp;
pub use parse::ast::BinaryOp;

pub type Ident = String;

pub struct File {
    pub variables: HashMap<String, Var>, // Globals
    pub function_declarations: HashMap<String, FunctionProto>, // Symbols
    pub types: HashMap<String, Struct>,
    pub function_definitions: Vec<Function>,
}

#[derive(Clone)]
pub struct Var {
    pub typ: Type,
    pub name: String,
}

#[derive(Clone,PartialEq)]
pub enum Type {
    Int,
    Struct(String),
    Void,
    Null,
}

pub struct FunctionProto {
    pub ret_type: Type,
    pub name : String, // Keep the symbol under hand.
    pub params_type : Vec<Type>,
}

pub struct Function {
    pub ret_type: Type,
    pub name : String, // Keep the symbol under hand.
    pub params : Vec<String>,
    pub vars : HashMap<String, Var>,
    pub blk : Bloc,
}

#[derive(Clone)]
pub struct Struct {
    pub members: Vec<Var>,
    pub index: HashMap<String, usize>,
}

pub struct Bloc {
    pub decls: HashMap<String, Var>,
    pub stmts: Vec<Statement>,
}

/*pub struct Statement {
    pub ret_type: Type,
    pub kind: StatementKind,
}*/

pub enum Statement {
    Noop,
    Expr(Expression),
    If(Expression, Box<Statement>),
    IfElse(Expression, Box<Statement>, Box<Statement>),
    While(Expression, Box<Statement>),
    Return(Expression),
    Bloc(Bloc),
}

pub struct Expression {
    pub typ: Type,
    pub kind: ExprKind
}

pub enum ExprKind {
    Const(i64),
    Lvalue(String),
    MembDeref(Box<Expression>, String),
    Call(String, Vec<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Sizeof(String),
}
