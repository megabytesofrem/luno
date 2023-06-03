pub mod ast;
pub mod operator;
pub mod pattern;

use std::iter::Peekable;

use crate::{
    error::SyntaxError,
    lexer::{lex_tokens, Token, TokenKind},
};

use self::ast::*;

/// Wrap the lexer in a `Peekable` iterator so we can peek at the next token
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

    fn text_span(&self, span: std::ops::Range<usize>) -> &'a str {
        &self.src[span]
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().cloned()
    }

    fn bump(&mut self) -> Option<Token> {
        let token = self.lexer.next();
        if let Some(token) = token.clone() {
            self.pos = token.span.end;
        }
        token
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, SyntaxError> {
        let token = self.bump();
        if let Some(token) = token {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(SyntaxError::UnexpectedToken {
                    expected: kind,
                    found: token.kind,
                    location: token.span,
                })
            }
        } else {
            Err(SyntaxError::UnexpectedEOF {
                expected: kind,
                location: self.pos..self.pos,
            })
        }
    }

    fn eat(&mut self, tokens: &'static [TokenKind]) -> Result<Token, SyntaxError> {
        if let Some(token) = self.peek() {
            if tokens.contains(&token.kind) {
                return Ok(self.bump().unwrap());
            }
        }

        Err(SyntaxError::UnexpectedToken {
            expected: TokenKind::EOF,
            found: self.peek().map(|t| t.kind).unwrap_or(TokenKind::EOF),
            location: self.pos..self.pos,
        })
    }
}

/// Seperate grammar rules from the parser primitives so we can clean up the parser
/// implementation
impl<'a> Parser<'a> {
    fn parse_ident(&mut self) -> Result<String, SyntaxError> {
        let token = self.expect(TokenKind::Ident)?;
        Ok(self.text_span(token.span).to_string())
    }

    fn parse_int(&mut self) -> Result<i32, SyntaxError> {
        let token = self.expect(TokenKind::Int)?;
        Ok(self.text_span(token.span).parse().unwrap())
    }

    fn parse_string(&mut self) -> Result<String, SyntaxError> {
        let token = self.expect(TokenKind::String)?;
        Ok(self.text_span(token.span).trim_matches('"').to_string())
    }

    fn parse_literal(&mut self) -> Result<Expr, SyntaxError> {
        let token = self.peek();
        if let Some(token) = token {
            match token.kind {
                TokenKind::Minus | TokenKind::Plus => {
                    self.bump();
                    Ok(Expr::Unary(
                        token.kind.into(),
                        Box::new(self.parse_literal()?),
                    ))
                }
                TokenKind::Int => Ok(Expr::Int(self.parse_int()?)),
                TokenKind::String => Ok(Expr::String(self.parse_string()?)),
                TokenKind::Ident => {
                    self.bump();
                    if let Some(next_token) = self.peek() {
                        match next_token.kind {
                            TokenKind::LParen => {
                                self.parse_call(self.text_span(token.span).to_string())
                            }
                            _ => Ok(Expr::Ident(self.text_span(token.span).to_string())),
                        }
                    } else {
                        Ok(Expr::Ident(self.text_span(token.span).to_string()))
                    }
                }
                TokenKind::LSquare => self.parse_array(),
                TokenKind::LBrace => self.parse_table(),
                _ => Err(SyntaxError::UnexpectedToken {
                    expected: TokenKind::EOF,
                    found: token.kind,
                    location: token.span,
                }),
            }
        } else {
            // We've reached the end of the file without finding a token
            Err(SyntaxError::UnexpectedEOF {
                expected: TokenKind::EOF,
                location: self.pos..self.pos,
            })
        }
    }

    fn parse_array(&mut self) -> Result<Expr, SyntaxError> {
        let mut items = Vec::new();
        self.expect(TokenKind::LSquare)?;
        loop {
            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RSquare {
                    self.bump();
                    break;
                } else {
                    items.push(self.parse_literal()?);
                    if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                        self.bump();
                    }
                }
            }
        }

        Ok(Expr::Array(items))
    }

    fn parse_table(&mut self) -> Result<Expr, SyntaxError> {
        let mut items = Vec::new();
        self.expect(TokenKind::LBrace)?;
        loop {
            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RBrace {
                    self.bump();
                    break;
                } else {
                    let key = self.parse_ident()?;
                    self.expect(TokenKind::Equal)?;
                    let value = self.parse_expr()?;
                    items.push((key, value));
                    if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                        self.bump();
                    }
                }
            }
        }

        Ok(Expr::Table(items))
    }

    fn parse_call(&mut self, ident: String) -> Result<Expr, SyntaxError> {
        let mut args = Vec::new();
        self.expect(TokenKind::LParen)?;
        loop {
            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RParen {
                    self.bump();
                    break;
                } else {
                    args.push(self.parse_literal()?);
                    if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                        self.bump();
                    }
                }
            }
        }

        Ok(Expr::Call(ident, args))
    }

    // Expression parsing hell
    // We need to parse expressions in order of precedence (from lowest to highest)
    fn parse_factor(&mut self) -> Result<Expr, SyntaxError> {
        // factor := literal ( ( "*" | "/" ) literal )*
        let mut left = self.parse_literal()?;
        while let Ok(op) = self.eat(&[TokenKind::Star, TokenKind::Slash]) {
            let right = self.parse_literal()?;
            left = Expr::Binary(op.kind.into(), Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, SyntaxError> {
        // term := factor ( ( "+" | "-" ) factor )*
        let mut left = self.parse_factor()?;
        while let Ok(op) = self.eat(&[TokenKind::Plus, TokenKind::Minus]) {
            let right = self.parse_factor()?;
            left = Expr::Binary(op.kind.into(), Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_compare(&mut self) -> Result<Expr, SyntaxError> {
        // compare := term ( ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) term )*
        let mut left = self.parse_term()?;
        let ops = &[
            TokenKind::Eq,
            TokenKind::NotEq,
            TokenKind::Lt,
            TokenKind::LtEq,
            TokenKind::Gt,
            TokenKind::GtEq,
        ];
        while let Ok(op) = self.eat(ops) {
            let right = self.parse_term()?;
            left = Expr::Binary(op.kind.into(), Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub fn parse_expr(&mut self) -> Result<Expr, SyntaxError> {
        // expr := ("-" expr) | compare
        if self.peek().is_some() {
            self.parse_compare()
        } else {
            Err(SyntaxError::UnexpectedEOF {
                expected: TokenKind::EOF,
                location: self.pos..self.pos,
            })
        }
    }

    // Statement parsing

    fn parse_stmt_expr(&mut self) -> Result<Stmt, SyntaxError> {
        let expr = self.parse_expr()?;
        Ok(Stmt::Expr { expr })
    }

    fn parse_import(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect(TokenKind::Import)?;
        let path = self.parse_dotted_path()?;

        // Optionally parse a "as" clause
        let alias = if self.peek().map(|t| t.kind) == Some(TokenKind::As) {
            self.bump();
            Some(self.parse_ident()?)
        } else {
            None
        };

        Ok(Stmt::Import {
            path,
            alias_name: alias,
        })
    }

    fn parse_type_name(&mut self) -> Result<Type, SyntaxError> {
        let token = self.expect(TokenKind::Ident)?;
        match self.text_span(token.span.clone()) {
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "string" => Ok(Type::String),
            "bool" => Ok(Type::Bool),
            _ => Err(SyntaxError::UnexpectedToken {
                expected: TokenKind::Ident,
                found: token.kind,
                location: token.span,
            }),
        }
    }

    fn parse_var(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect(TokenKind::Var)?;
        let ident = self.parse_ident()?;
        self.expect(TokenKind::Equal)?;
        let value = self.parse_expr()?;
        Ok(Stmt::VarDeclare {
            type_: Type::Unspecified,
            name: ident,
            value,
        })
    }

    fn parse_assignment(&mut self, ident: String) -> Result<Stmt, SyntaxError> {
        self.expect(TokenKind::Equal)?;
        let value = self.parse_expr()?;
        Ok(Stmt::Assign { name: ident, value })
    }

    fn parse_if(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect(TokenKind::If)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::Then)?;
        let then_block = self.parse_block()?;

        // Parse optional else block
        let else_block = if let Some(token) = self.peek() {
            if token.kind == TokenKind::Else {
                self.bump();
                Some(self.parse_block()?)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Stmt::If {
            cond,
            then_branch: Box::new(then_block),
            else_branch: else_block.map(Box::new),
        })
    }

    fn parse_for(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect(TokenKind::For)?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let range = self.parse_expr()?;
        self.expect(TokenKind::Do)?;
        let block = self.parse_block()?;
        Ok(Stmt::For {
            name,
            range,
            block: Box::new(block),
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect(TokenKind::While)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::Do)?;
        let block = self.parse_block()?;
        Ok(Stmt::While {
            cond,
            block: Box::new(block),
        })
    }

    fn parse_fn(&mut self) -> Result<Stmt, SyntaxError> {
        let mut params = Vec::new();

        self.expect(TokenKind::Fn)?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::LParen)?;

        // Parse optional parameters
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RParen {
                break;
            }

            let name = self.parse_ident()?;
            self.expect(TokenKind::Colon)?;
            let type_ = self.parse_type_name()?;
            params.push((name, type_));

            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RParen {
                    break;
                }
                self.expect(TokenKind::Comma)?;
            }
        }

        // Parse return type
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::FatArrow)?;
        let return_type = self.parse_type_name()?;

        self.expect(TokenKind::Do)?;
        let block = self.parse_block()?;

        self.bump();

        Ok(Stmt::Function {
            name,
            params,
            block: Box::new(block),
            return_type,
        })
    }

    fn parse_return(&mut self) -> Result<Stmt, SyntaxError> {
        self.expect(TokenKind::Return)?;
        let expr = self.parse_expr()?;
        Ok(Stmt::Return { expr })
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let token = self.peek().ok_or(SyntaxError::UnexpectedEOF {
            expected: TokenKind::EOF,
            location: self.pos..self.pos,
        })?;

        match token.kind {
            TokenKind::Ident => {
                let ident = self.parse_ident()?;
                if let Some(token) = self.peek() {
                    println!("token: {:?}", token.kind);
                    if token.kind == TokenKind::LParen {
                        let expr = self.parse_call(ident)?;
                        Ok(Stmt::Expr { expr })
                    } else {
                        self.parse_assignment(ident)
                    }
                } else {
                    Err(SyntaxError::UnexpectedEOF {
                        expected: TokenKind::EOF,
                        location: self.pos..self.pos,
                    })
                }
            }
            TokenKind::Import => self.parse_import(),
            TokenKind::Var => self.parse_var(),
            TokenKind::If => self.parse_if(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Fn => self.parse_fn(),
            TokenKind::Return => self.parse_return(),
            _ => self.parse_stmt_expr(),
        }
    }

    pub fn parse_block(&mut self) -> Result<Stmt, SyntaxError> {
        let mut stmts = Vec::new();
        while !&[TokenKind::Else, TokenKind::End].contains(&self.peek().unwrap().kind) {
            stmts.push(self.parse_stmt()?);
        }

        Ok(Stmt::Block { stmts })
    }

    /// Parse a complete program into an AST
    pub fn parse(&mut self) -> Result<Ast, SyntaxError> {
        let mut stmts = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::EOF {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }

        // Loop through and find all imports
        let mut imports = Vec::new();
        for stmt in stmts.iter() {
            if let Stmt::Import { path, .. } = stmt {
                imports.push(path.clone());
            }
        }

        Ok(Ast {
            import_list: imports,
            stmts,
        })
    }
}

#[cfg(test)]
mod parser_tests {
    use crate::parser::operator::Operator;

    use super::*;

    #[test]
    fn parse_expr() {
        let mut parser = Parser::new(r#"{twilight = 1, rarity = 2}"#);
        let ast = parser.parse_expr().unwrap();

        assert_eq!(
            ast,
            Expr::Table(vec![
                ("twilight".to_string(), Expr::Int(1)),
                ("rarity".to_string(), Expr::Int(2))
            ])
        )
    }

    #[test]
    fn parse_array() {
        let mut parser = Parser::new(r#"[1,2,3]"#);
        let ast = parser.parse_literal().unwrap();

        assert_eq!(
            ast,
            Expr::Array(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)])
        )
    }

    #[test]
    fn parse_stmt() {
        let mut parser = Parser::new(
            r#"
fn abc(foo: int) => int do
    var a = 1
    var b = 2
    return a + b
end
"#,
        );
        let ast = parser.parse_stmt().unwrap();

        assert_eq!(
            ast,
            Stmt::Function {
                name: "abc".to_string(),
                params: vec![("foo".to_string(), Type::Int)],
                return_type: Type::Int,
                block: Box::new(Stmt::Block {
                    stmts: vec![
                        Stmt::VarDeclare {
                            type_: Type::Unspecified,
                            name: "a".to_string(),
                            value: Expr::Int(1)
                        },
                        Stmt::VarDeclare {
                            type_: Type::Unspecified,
                            name: "b".to_string(),
                            value: Expr::Int(2)
                        },
                        Stmt::Return {
                            expr: Expr::Binary(
                                Operator::Add,
                                Box::new(Expr::Ident("a".to_string())),
                                Box::new(Expr::Ident("b".to_string()))
                            )
                        }
                    ]
                })
            }
        )
    }

    #[test]
    fn parse_ast() {
        let mut parser = Parser::new(
            r#"
import foo_mod.bar as bar

var a = 1
var x = 2
"#,
        );

        let ast = parser.parse().unwrap();

        assert_eq!(
            ast,
            Ast {
                import_list: vec![DottedPath {
                    path_components: vec!["foo_mod".to_string(), "bar".to_string()]
                }],
                stmts: vec![
                    Stmt::Import {
                        path: DottedPath {
                            path_components: vec!["foo_mod".to_string(), "bar".to_string()]
                        },
                        alias_name: Some("bar".to_string())
                    },
                    Stmt::VarDeclare {
                        type_: Type::Unspecified,
                        name: "a".to_string(),
                        value: Expr::Int(1)
                    },
                    Stmt::VarDeclare {
                        type_: Type::Unspecified,
                        name: "x".to_string(),
                        value: Expr::Int(2)
                    }
                ]
            }
        )
    }
}
