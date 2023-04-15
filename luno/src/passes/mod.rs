use crate::{ast::AST, error::SyntaxError};

pub mod scope;
pub mod type_checker;

/// Perform all semantic analysis passes on the AST
pub fn perform_passes(ast: &mut AST) -> Result<(), SyntaxError> {
    let mut scope_resolver = scope::ScopeResolver::new();
    scope_resolver.visit_ast(ast)?;

    let mut type_checker = type_checker::TypeChecker::new();
    type_checker.visit_ast(ast)?;

    Ok(())
}
