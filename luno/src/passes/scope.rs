///! Tree walk scope resolver.
use std::collections::HashMap;

use crate::{
    ast::{Block, Stmt, Type, AST},
    error::SyntaxError,
};

#[derive(Debug, Clone)]
pub struct Scope {
    pub symbols: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
/// A scope resolver is used to resolve variables by walking
/// the AST and keeping track of the current scope.
pub struct ScopeResolver {
    scopes: Vec<Scope>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}

impl ScopeResolver {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    fn resolve(&mut self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.symbols.get(name) {
                return Some(t.clone());
            }
        }

        None
    }

    pub fn get_curr_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn insert(&mut self, name: &str, type_: Type) {
        self.scopes
            .last_mut()
            .unwrap()
            .symbols
            .insert(name.to_string(), type_);
    }

    /// Visit the AST and resolve all variables
    pub fn visit_ast(&mut self, ast: &AST) -> Result<(), SyntaxError> {
        ast.stmts.iter().try_for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), SyntaxError> {
        match stmt {
            Stmt::VarDeclare { type_, name, .. } => {
                // Keep track of the variable in the current scope
                self.insert(name, type_.clone());
            }
            Stmt::Assign { name, .. } => {
                if self.resolve(name).is_none() {
                    return Err(SyntaxError::NotInScope { name: name.clone() });
                }
            }
            Stmt::If {
                cond: _,
                then_block,
                else_block,
            } => {
                self.visit_block(then_block)?;
                if let Some(else_block) = else_block {
                    self.visit_block(else_block)?;
                }
            }
            Stmt::For {
                name,
                range: _,
                block,
            } => {
                // We don't know the type of the range, so we just assume it's unspecified.
                self.insert(name, Type::Unspecified);

                // Visit the block recursively
                self.visit_block(block)?;
            }
            Stmt::While { cond: _, block } => {
                self.visit_block(block)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn visit_block(&mut self, ast: &Block) -> Result<(), SyntaxError> {
        for stmt in ast.stmts.iter() {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ScopeResolver {
    fn default() -> Self {
        Self::new()
    }
}
