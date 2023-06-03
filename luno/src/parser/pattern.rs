use crate::{error::SyntaxError, lexer::TokenKind};

use super::ast::DottedPath;
use super::{ast::Expr, Parser};

#[allow(dead_code)]
impl<'a> Parser<'a> {
    /// Parse a dotted path, e.g. `foo.bar.baz`
    pub fn parse_dotted_path(&mut self) -> Result<DottedPath, SyntaxError> {
        let mut path_components = Vec::new();
        loop {
            if let Some(token) = self.peek() {
                if token.kind == TokenKind::Dot {
                    self.bump();
                } else if token.kind == TokenKind::Ident {
                    path_components.push(self.parse_ident()?);
                } else {
                    break;
                }
            }
        }

        Ok(DottedPath { path_components })
    }

    /// Parse a slice expression, e.g. `foo[bar]`
    pub fn parse_slice(&mut self) -> Result<Expr, SyntaxError> {
        let expr = self.parse_expr()?;

        self.expect(TokenKind::LSquare)?;
        let index = self.parse_expr()?;
        self.expect(TokenKind::RSquare)?;

        Ok(Expr::Slice(Box::new(expr), Box::new(index)))
    }
}
