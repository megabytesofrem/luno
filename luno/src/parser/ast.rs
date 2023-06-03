use crate::lexer::TokenKind;

use super::operator::Operator;

pub type FunctionID = usize;

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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Ident(String),

    Array(Vec<Expr>),
    Slice(Box<Expr>, Box<Expr>), // array[expr]
    Table(Vec<(String, Expr)>),
    Unary(Operator, Box<Expr>),
    Binary(Operator, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Expr::Int(lhs), Expr::Int(rhs)) => lhs.partial_cmp(rhs),
            (Expr::Float(lhs), Expr::Float(rhs)) => lhs.partial_cmp(rhs),
            (Expr::String(lhs), Expr::String(rhs)) => lhs.partial_cmp(rhs),
            (Expr::Bool(lhs), Expr::Bool(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block {
        stmts: Vec<Stmt>,
    },
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
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    For {
        name: String,
        range: Expr,
        block: Box<Stmt>,
    },
    While {
        cond: Expr,
        block: Box<Stmt>,
    },
    Function {
        name: String,
        params: Vec<(String, Type)>,
        return_type: Type,
        block: Box<Stmt>,
    },
    Import {
        path: DottedPath,

        // Optional alias for the import
        alias_name: Option<String>,
    },
    Return {
        expr: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct DottedPath {
    /// List of path components
    pub path_components: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    /// List of import statements collected
    pub import_list: Vec<DottedPath>,

    /// List of statements in the AST
    pub stmts: Vec<Stmt>,
}

impl From<TokenKind> for Operator {
    /// Convert a token kind to an operator
    fn from(token: TokenKind) -> Self {
        match token {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Star => Self::Mul,
            TokenKind::Slash => Self::Div,
            TokenKind::Eq => Self::Eq,
            TokenKind::NotEq => Self::NotEq,
            TokenKind::Lt => Self::Lt,
            TokenKind::Gt => Self::Gt,
            TokenKind::LtEq => Self::LtEq,
            TokenKind::GtEq => Self::GtEq,
            _ => unreachable!(),
        }
    }
}
