
pub type Ident = String;

#[derive(Clone, Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug)]
pub struct Node<T> { // Generic metadata for all the nodes in the tree.
    pub start: Location,
    pub stop: Location,
    pub t: T,
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
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

#[derive(Clone, Debug)]
pub enum Param {
    Int(Node<Ident>),
    Struct(Node<Ident>, Node<Ident>),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Int(i64),
    Ident(Node<Ident>),
    MembDeref(Box<Node<Expression>>, Node<Ident>),
    Call(Node<Ident>, Vec<Node<Expression>>),
    Unary(Node<UnaryOp>, Box<Node<Expression>>),
    Binary(Box<Node<Expression>>, Node<BinaryOp>, Box<Node<Expression>>),
    Sizeof(Node<Ident>),
    Parens(Box<Node<Expression>>),
}

pub type DeclType = (Node<Ident>, Vec<Node<DeclVar>>);



#[derive(Clone, Debug)]
pub enum DeclVar {
    Int(Vec<Node<Ident>>),
    Struct(Node<Ident>, Vec<Node<Ident>>),
}

// Function declaration
#[derive(Clone, Debug)]
pub enum DeclFunc {
    Int(Node<Ident>, Vec<Node<Param>>, Node<Bloc>),
    Struct(Node<Ident>, Node<Ident>, Vec<Node<Param>>, Node<Bloc>),
}


// Declarations
#[derive(Clone, Debug)]
pub enum Declaration {
    Var(Node<DeclVar>),
    Type(Node<DeclType>),
    Func(Node<DeclFunc>),
}


pub type Bloc = (Vec<Node<DeclVar>>, Vec<Node<Statement>>);

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(Node<Expression>),
    If(Node<Expression>, Box<Node<Statement>>),
    IfElse(Node<Expression>, Box<Node<Statement>>, Box<Node<Statement>>),
    While(Node<Expression>, Box<Node<Statement>>),
    Return(Node<Expression>),
    Bloc(Node<Bloc>),
    Noop,
}
