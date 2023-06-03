use crate::parser::ast::*;

pub trait Visitor<T> {
    fn visit(&mut self, ast: &Ast) -> T;
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
    fn visit_expr(&mut self, expr: &Expr) -> T;
}
