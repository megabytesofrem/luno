#![warn(clippy::correctness, clippy::style)]
pub mod error;
pub mod lexer;
pub mod parser;

/// Semantic analysis passes
pub mod passes;
