use crate::lexer::TokenKind;

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool),

    Array(Vec<Expr>),
    Table(Vec<(Expr, Expr)>),
    Binary(TokenKind, Box<Expr>, Box<Expr>),
    Ident(String),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    String,
    Float,
    Bool,
    Array(Box<Type>),

    /// A table type similar to a dictionary in Python
    Table(Box<Type>, Box<Type>),

    /// A function type
    Function(Vec<Type>, Box<Type>),

    Unspecified,
}

#[derive(Debug, Clone)]
pub struct Block {
    /// List of statements in the block
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDeclare {
        type_: Type,
        name: String,
        value: Expr,
    },
    Assign {
        name: String,
        value: Expr,
    },
    Expr {
        expr: Expr,
    },
    If {
        cond: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    For {
        name: String,
        range: Expr,
        block: Block,
    },
    While {
        cond: Expr,
        block: Block,
    },
    Function {
        name: String,
        params: Vec<(Type, String)>,
        return_type: Type,
        block: Block,
    },
    Import {
        path: String,
    },
    Return {
        value: Expr,
    },
}

#[derive(Debug, Clone)]
pub struct AST {
    /// List of statements in the AST
    pub stmts: Vec<Stmt>,
}
