use std::iter::Peekable;

use crate::{
    ast::{Block, Expr, Stmt, Type},
    error::ParseError,
    lexer::{lex_tokens, Token, TokenKind},
};

// Wrap the lexer in a Peekable so we can peek at the next token
type PeekableLexer<'a> = Peekable<Box<dyn Iterator<Item = Token> + 'a>>;

pub struct Parser<'a> {
    src: &'a str,
    pos: usize,
    lexer: PeekableLexer<'a>,
}

#[allow(dead_code)]
impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            pos: 0,
            lexer: (Box::new(lex_tokens(src)) as Box<dyn Iterator<Item = Token>>).peekable(),
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    #[allow(clippy::should_implement_trait)]
    fn next(&mut self) -> Option<Token> {
        let token = self.lexer.next();
        if let Some(token) = token.clone() {
            self.pos = token.span.end;
        }
        token
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        let token = self.next();
        if let Some(token) = token {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: kind,
                    found: token.kind,
                    location: token.span,
                })
            }
        } else {
            Err(ParseError::UnexpectedEOF {
                expected: kind,
                location: self.pos..self.pos,
            })
        }
    }

    fn eat(&mut self, tokens: &'static [TokenKind]) -> Result<Token, ParseError> {
        if let Some(token) = self.peek() {
            if tokens.contains(&token.kind) {
                return Ok(self.next().unwrap());
            }
        }

        Err(ParseError::UnexpectedToken {
            expected: TokenKind::EOF,
            found: self
                .peek()
                .map(|t| t.clone().kind)
                .unwrap_or(TokenKind::EOF),
            location: self.pos..self.pos,
        })
    }

    // Parser methods
    pub(crate) fn parse_ident(&mut self) -> Result<String, ParseError> {
        let token = self.expect(TokenKind::Ident)?;
        Ok(self.src[token.span].to_string())
    }

    pub(crate) fn parse_integer(&mut self) -> Result<i32, ParseError> {
        let token = self.expect(TokenKind::Integer)?;
        Ok(self.src[token.span].parse().unwrap())
    }

    pub(crate) fn parse_string(&mut self) -> Result<String, ParseError> {
        let token = self.expect(TokenKind::String)?;
        Ok(self.src[token.span].to_string())
    }

    pub(crate) fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let name = self.parse_ident()?;
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();
        loop {
            let token = self.next();
            if let Some(token) = token {
                if token.kind == TokenKind::RParen {
                    break;
                } else {
                    args.push(self.parse_literal()?);
                }
            } else {
                return Err(ParseError::UnexpectedEOF {
                    expected: TokenKind::RParen,
                    location: self.pos..self.pos,
                });
            }
        }
        Ok(Expr::Call(name, args))
    }

    pub(crate) fn parse_literal(&mut self) -> Result<Expr, ParseError> {
        let token = self.next();
        if let Some(token) = token {
            match token.kind {
                TokenKind::Integer => self.parse_integer().map(Expr::Int),
                TokenKind::String => self.parse_string().map(Expr::String),
                TokenKind::Ident => {
                    let next = self.peek();
                    if let Some(next) = next {
                        if next.kind == TokenKind::LParen {
                            // Parse a function call
                            self.parse_call()
                        } else {
                            // Parse an identifier
                            Ok(Expr::Ident(self.src[token.span].to_string()))
                        }
                    } else {
                        // Return unexpected EOF error
                        Err(ParseError::UnexpectedEOF {
                            expected: TokenKind::RSquare,
                            location: self.pos..self.pos,
                        })
                    }
                }

                TokenKind::LSquare => {
                    // Parse an array literal
                    let mut exprs = Vec::new();
                    loop {
                        let token = self.next();
                        if let Some(token) = token {
                            if token.kind == TokenKind::RSquare {
                                break;
                            } else {
                                exprs.push(self.parse_literal()?);
                            }
                        } else {
                            return Err(ParseError::UnexpectedEOF {
                                expected: TokenKind::RSquare,
                                location: self.pos..self.pos,
                            });
                        }
                    }
                    Ok(Expr::Array(exprs))
                }
                _ => Err(ParseError::UnexpectedToken {
                    expected: TokenKind::Literal,
                    found: token.kind,
                    location: token.span,
                }),
            }
        } else {
            Err(ParseError::UnexpectedEOF {
                expected: TokenKind::Integer,
                location: self.pos..self.pos,
            })
        }
    }

    // Expression parsing
    pub(crate) fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_literal()?;
        while let Ok(op) = self.eat(&[TokenKind::Star, TokenKind::Slash]) {
            let right = self.parse_literal()?;
            left = Expr::Binary(op.kind, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub(crate) fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;
        while let Ok(op) = self.eat(&[TokenKind::Plus, TokenKind::Minus]) {
            let right = self.parse_factor()?;
            left = Expr::Binary(op.kind, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub(crate) fn parse_compare(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_term()?;
        while let Ok(op) = self.eat(&[
            TokenKind::Equal,
            TokenKind::Eq,
            TokenKind::NotEq,
            TokenKind::Lt,
            TokenKind::LtEq,
            TokenKind::Gt,
            TokenKind::GtEq,
        ]) {
            let right = self.parse_term()?;
            left = Expr::Binary(op.kind, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_compare()
    }

    // Statement parsing

    pub(crate) fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self.next().unwrap();
        match next_token.kind {
            TokenKind::Var => {
                let name = self.parse_ident()?;
                self.expect(TokenKind::Colon)?;
                let type_ = self.parse_type_name()?;
                self.expect(TokenKind::Equal)?;
                let value = self.parse_expr()?;
                Ok(Stmt::VarDeclare { type_, name, value })
            }
            TokenKind::Ident => {
                let name = self.src[next_token.span].to_string();
                self.expect(TokenKind::Equal)?;
                let value = self.parse_expr()?;
                Ok(Stmt::Assign { name, value })
            }
            TokenKind::If => {
                let cond = self.parse_expr()?;
                self.expect(TokenKind::Then)?;
                let then_block = self.parse_block()?;
                let else_block = if self.eat(&[TokenKind::Else]).is_ok() {
                    Some(self.parse_block()?)
                } else {
                    None
                };
                Ok(Stmt::If {
                    cond,
                    then_block,
                    else_block,
                })
            }
            TokenKind::While => {
                let cond = self.parse_expr()?;
                self.expect(TokenKind::Do)?;
                let block = self.parse_block()?;
                Ok(Stmt::While { cond, block })
            }

            TokenKind::Ret => {
                let value = self.parse_expr()?;
                Ok(Stmt::Ret { value })
            }

            _ => todo!(),
        }
    }

    pub(crate) fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut stmts = Vec::new();
        while self.peek().unwrap().kind != TokenKind::End {
            stmts.push(self.parse_stmt()?);
        }

        self.next();
        Ok(Block { stmts })
    }

    pub(crate) fn parse_type_name(&mut self) -> Result<Type, ParseError> {
        match self.parse_ident()?.as_str() {
            "int" => Ok(Type::Int),
            "string" => Ok(Type::String),
            "bool" => Ok(Type::Bool),
            _ => Err(ParseError::UnknownType),
        }
    }

    pub(crate) fn parse_var_decl(&mut self) -> Result<Stmt, ParseError> {
        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let type_ = self.parse_type_name()?;
        self.expect(TokenKind::Equal)?;
        let value = self.parse_expr()?;
        Ok(Stmt::VarDeclare { type_, name, value })
    }

    pub(crate) fn parse_var_assign(&mut self) -> Result<Stmt, ParseError> {
        let name = self.parse_ident()?;
        self.expect(TokenKind::Equal)?;
        let value = self.parse_expr()?;
        Ok(Stmt::Assign { name, value })
    }
}
