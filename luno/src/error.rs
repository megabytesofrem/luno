use crate::{
    lexer::{Location, TokenKind},
    parser::ast::Type,
};

use thiserror::Error;

#[derive(Debug, PartialEq, Error)]
pub enum SyntaxError {
    #[error("Expected {expected:?}, but got {found:?}")]
    UnexpectedToken {
        location: Location,
        found: TokenKind,
        expected: TokenKind,
    },

    #[error("Expected {expected:?}, but got EOF")]
    UnexpectedEOF {
        location: Location,
        expected: TokenKind,
    },
}

#[derive(Debug, PartialEq, Error)]
pub enum RuntimeError {
    #[error("Type mismatch: expected {expected:?}, but got {found:?}")]
    TypeMismatch { found: Type, expected: Type },

    #[error("Variable {name} is not in scope")]
    NotInScope { name: String },

    #[error("Variable {name} is already in scope")]
    AlreadyInScope { name: String },

    #[error("Expected {expected} arguments, but got {actual}")]
    WrongNumberOfArgs { expected: usize, actual: usize },

    #[error("{name} is not a function")]
    NotAFunction { name: String },

    #[error("Unknown type")]
    UnknownType,
}
