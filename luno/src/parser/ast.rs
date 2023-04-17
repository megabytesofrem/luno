use crate::lexer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool),

    Array(Vec<Expr>),
    Table(Vec<(String, Expr)>),
    Unary(Operator, Box<Expr>),
    Binary(Operator, Box<Expr>, Box<Expr>),
    Ident(String),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DottedPath {
    /// List of path components
    pub path_components: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// List of statements in the block
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
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
        params: Vec<(String, Type)>,
        return_type: Type,
        block: Block,
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
