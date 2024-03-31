#[derive(Debug)]
pub enum VarType {
    Int,
    Bool,
    Char,
}

#[derive(Debug)]
pub enum ReturnType {
    Int,
    Bool,
    Char,
    Void,
}

#[derive(Debug)]
pub struct Program {
    pub header: bool,
    pub decl: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    VarDecl(VarDecl),
    Fn(ReturnType, String, Vec<Parameter>, Option<Scope>),
}

#[derive(Debug)]
pub struct VarDecl {
    pub t: VarType,
    pub variables: Vec<VariableList>,
}

#[derive(Debug)]
pub enum VariableList {
    Basic(String),
    List(String, i32),
}

#[derive(Debug)]
pub struct Scope {
    pub vardecl: Vec<VarDecl>,
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    If(Expr, Vec<Statement>),
    IfElse(Expr, Vec<Statement>, Vec<Statement>),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Statement>),
    Continue,
    Break,
    Return(Option<Expr>),
    Scope(Scope),
}

#[derive(Debug)]
pub struct Parameter {
    pub t: VarType,
    pub name: String,
}

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    Char(char),
    Bool(bool),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Lte(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Gte(Box<Expr>, Box<Expr>),
    Variable(Box<Variable>),
    FunctionCall(String, Vec<Expr>),
    Assignment(Box<Variable>, Box<Expr>),
}

#[derive(Debug)]
pub enum Variable {
    Basic(String),
    List(String, Expr),
}
