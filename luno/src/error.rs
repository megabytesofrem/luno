use core::fmt;

use crate::lexer::{Location, TokenKind};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        location: Location,
        found: TokenKind,
        expected: TokenKind,
    },

    UnexpectedEOF {
        location: Location,
        expected: TokenKind,
    },

    UnknownType,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                location: _,
                found,
                expected,
            } => {
                write!(f, "Expected {expected:?}, but got {found:?}")
            }
            ParseError::UnexpectedEOF {
                location: _,
                expected,
            } => {
                write!(f, "Expected {expected:?}, but got EOF")
            }
            ParseError::UnknownType => {
                write!(f, "Unknown type")
            }
        }
    }
}
