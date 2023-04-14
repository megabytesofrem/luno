use crate::lexer::TokenKind;

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i32),
    Float(f32),
    String(String),

    Array(Vec<Expr>),
    Binary(TokenKind, Box<Expr>, Box<Expr>),
    Ident(String),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    String,
    Float,
    Bool,
    Array(Box<Type>),

    /// A function type
    Fn(Vec<Type>, Box<Type>),

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
    Import {
        path: String,
    },
    Ret {
        value: Expr,
    },
}

#[derive(Debug, Clone)]
pub struct Program {
    /// List of statements in the program
    pub stmts: Vec<Stmt>,
}
