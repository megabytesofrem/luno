use crate::parser::ast::*;

use super::scope::ScopePass;

/// A trait for callbacks that are called during AST traversal.
pub trait AstTraversalCallback {
    fn on_visit_ast(&mut self, ast: &Ast);
    fn on_visit_stmt(&mut self, stmt: &Stmt);
    fn on_visit_block(&mut self, stmt: &Stmt);
}

/// A trait for callbacks that are called during scope checking.
///
/// This is a separate trait from `AstTraversalCallback` because
/// the scope checking pass needs to pass the scope to the callback.
pub trait ScopePassCallback {
    fn on_visit_ast(&mut self, ast: &Ast, scope: &ScopePass);
    fn on_visit_stmt(&mut self, stmt: &Stmt, scope: &ScopePass);
    fn on_visit_block(&mut self, stmt: &Stmt, scope: &ScopePass);
    fn on_exit_block(&mut self, stmt: &Stmt, scope: &ScopePass);
}
