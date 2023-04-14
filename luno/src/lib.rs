#![warn(clippy::correctness, clippy::style)]
pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

/// Semantic analysis passes
pub mod passes;
