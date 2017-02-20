use std::collections::HashMap;
pub use parse::ast::UnaryOp;
pub use parse::ast::BinaryOp;

pub type Ident = String;

#[derive(Debug)]
pub struct File {
    pub variables: HashMap<String, Var>, // Globals
    pub function_declarations: HashMap<String, FunctionProto>, // Symbols
    pub types: HashMap<String, Struct>,
    pub function_definitions: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub typ: Type,
    pub name: String,
}

#[derive(Clone,PartialEq, Debug)]
pub enum Type {
    Int,
    Struct(String),
    Void,
    Null,
}

#[derive(Debug)]
pub struct FunctionProto {
    pub ret_type: Type,
    pub name : String, // Keep the symbol under hand.
    pub params_type : Vec<Type>,
}

#[derive(Debug)]
pub struct Function {
    pub ret_type: Type,
    pub name : String, // Keep the symbol under hand.
    pub params : Vec<String>,
    pub vars : HashMap<String, Var>,
    pub blk : Bloc,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub members: Vec<Var>,
    pub index: HashMap<String, usize>,
}

#[derive(Debug)]
pub struct Bloc {
    pub decls: HashMap<String, Var>,
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Noop,
    Expr(Expression),
    If(Expression, Box<Statement>),
    IfElse(Expression, Box<Statement>, Box<Statement>),
    While(Expression, Box<Statement>),
    Return(Expression),
    Bloc(Bloc),
}

#[derive(Debug)]
pub struct Expression {
    pub typ: Type,
    pub kind: ExprKind,
    _is_pure: bool,
}

impl Expression {
    pub fn is_expr_pure(&self) -> bool {
        self._is_pure
    }

    pub fn new(typ: Type, kind: ExprKind) -> Expression {
        let is_pure = kind.is_kind_pure();
        Expression{typ: typ, kind: kind, _is_pure: is_pure}
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Const(i64),
    Lvalue(String),
    MembDeref(Box<Expression>, String),
    Call(String, Vec<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Sizeof(String),
}

impl ExprKind {
    pub fn is_kind_pure(&self) -> bool {
        match self {
            &ExprKind::Const(_) => true,
            &ExprKind::Lvalue(_) => true,
            &ExprKind::MembDeref(ref be, _) => be.is_expr_pure(),
            &ExprKind::Call(..) => false, // This could change.
            &ExprKind::Unary(_, ref be) => be.is_expr_pure(),
            &ExprKind::Binary(ref be1, ref op, ref be2) => match op {
                &BinaryOp::Affect => false,
                _ => be1.is_expr_pure() && be2.is_expr_pure(),
            },
            &ExprKind::Sizeof(_) => true,
        }
    }
}
