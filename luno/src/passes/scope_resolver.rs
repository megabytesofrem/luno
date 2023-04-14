///! Tree walk scope resolver.
use std::collections::HashMap;

use crate::{
    ast::{Block, Program, Stmt, Type},
    error::ScopeError,
};

#[derive(Debug, Clone)]
pub struct Scope {
    pub vars: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
/// A scope resolver is used to resolve variables by walking
/// the AST and keeping track of the current scope.
pub struct ScopeResolver {
    scopes: Vec<Scope>,
}

impl Scope {
    pub fn new(&self) -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

impl ScopeResolver {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    fn resolve(&mut self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.vars.get(name) {
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
            .vars
            .insert(name.to_string(), type_);
    }

    /// Walk the AST and recursively resolve all the variables, entering blocks
    /// if necessary.
    pub fn walk_ast(&mut self, ast: &Program) -> Result<(), ScopeError> {
        ast.stmts.iter().try_for_each(|stmt| self.walk_stmt(stmt))
    }

    fn walk_stmt(&mut self, stmt: &Stmt) -> Result<(), ScopeError> {
        match stmt {
            Stmt::VarDeclare { type_, name, .. } => {
                // Keep track of the variable in the current scope
                self.insert(name, type_.clone());
            }
            Stmt::Assign { name, .. } => {
                if self.resolve(name).is_none() {
                    return Err(ScopeError::NotInScope { name: name.clone() });
                }
            }
            Stmt::If {
                cond: _,
                then_block,
                else_block,
            } => {
                self.walk_block(then_block)?;
                if let Some(else_block) = else_block {
                    self.walk_block(else_block)?;
                }
            }
            Stmt::For {
                name,
                range: _,
                block,
            } => {
                // We don't know the type of the range, so we just assume it's unspecified.
                self.insert(name, Type::Unspecified);

                // Walk the block recursively
                self.walk_block(block)?;
            }
            Stmt::While { cond: _, block } => {
                self.walk_block(block)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn walk_block(&mut self, ast: &Block) -> Result<(), ScopeError> {
        for stmt in ast.stmts.iter() {
            self.walk_stmt(stmt)?;
        }

        Ok(())
    }
}

impl Default for ScopeResolver {
    fn default() -> Self {
        Self::new()
    }
}
