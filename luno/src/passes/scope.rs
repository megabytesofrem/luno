use std::collections::HashMap;

use crate::{
    error::SyntaxError,
    parser::ast::{Ast, Stmt, Type},
};

use super::callback::ScopePassCallback;

/// A single scope within the scope stack.
#[derive(Debug, Clone)]
struct Scope {
    symbols: HashMap<String, Type>,
}

impl Scope {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    fn resolve_symbol(&self, name: &str) -> Option<Type> {
        self.symbols.get(name).cloned()
    }
}

/// A stack of scopes. The top of the stack is the current scope.
struct ScopeStack {
    stack: Vec<Scope>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            stack: vec![Scope::new()],
        }
    }

    pub fn resolve_symbol(&self, name: &str) -> Option<Type> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.resolve_symbol(name))
    }

    fn insert_symbol(&mut self, name: &str, type_: Type) {
        self.stack
            .last_mut()
            .unwrap()
            .symbols
            .insert(name.to_string(), type_);
    }

    fn push_scope(&mut self) {
        self.stack.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }
}

/// Pretty-print the scope hierarchy in a human-readable format.
struct ScopePrinter {
    indent_level: usize,
}

#[allow(unused_variables)]
impl ScopePassCallback for ScopePrinter {
    fn on_visit_ast(&mut self, ast: &Ast, pass: &ScopePass) {}

    fn on_visit_stmt(&mut self, stmt: &Stmt, pass: &ScopePass) {
        let indent = " ".repeat(self.indent_level * 4);

        match stmt {
            Stmt::VarDeclare { type_, name, value } => {
                println!("{}var {} : {:?} = {:?}", indent, name, type_, value)
            }
            Stmt::Assign { name, value } => println!("{}{} = {:?}", indent, name, value),
            _ => {}
        }
    }

    fn on_visit_block(&mut self, stmt: &Stmt, pass: &ScopePass) {
        let indent = " ".repeat(self.indent_level * 4);
        println!("{}{{", indent);
        self.indent_level += 1;
    }

    fn on_exit_block(&mut self, stmt: &Stmt, pass: &ScopePass) {
        self.indent_level -= 1;

        let indent = " ".repeat(self.indent_level * 4);
        println!("{}}}", indent);
    }
}

/// The scope pass. Performs name resolution and scope checking.
pub struct ScopePass {
    /// Stack of scopes to keep track of symbols within blocks
    scope_stack: ScopeStack,
}

impl ScopePass {
    pub fn visit_ast<CB>(&mut self, ast: &Ast, cb: &mut CB) -> Result<(), SyntaxError>
    where
        CB: ScopePassCallback,
    {
        for stmt in &ast.stmts {
            self.visit_stmt(stmt, cb)?;
        }

        Ok(())
    }

    pub(crate) fn visit_stmt<CB>(&mut self, stmt: &Stmt, cb: &mut CB) -> Result<(), SyntaxError>
    where
        CB: ScopePassCallback,
    {
        match stmt {
            Stmt::VarDeclare { type_, name, .. } => {
                self.scope_stack.insert_symbol(name, type_.clone())
            }
            Stmt::Assign { name, .. } => {
                if self.scope_stack.resolve_symbol(name).is_none() {
                    // Variable not in scope
                    return Err(SyntaxError::NotInScope { name: name.clone() });
                }
            }
            Stmt::If {
                cond: _,
                then_block,
                else_block,
            } => {
                self.visit_block(then_block, cb)?;
                if let Some(else_block) = else_block {
                    self.visit_block(else_block, cb)?;
                }
            }
            Stmt::Block { .. } => self.visit_block(stmt, cb)?,
            _ => todo!(),
        }

        cb.on_visit_stmt(stmt, self);

        Ok(())
    }

    pub(crate) fn visit_block<CB>(&mut self, stmt: &Stmt, cb: &mut CB) -> Result<(), SyntaxError>
    where
        CB: ScopePassCallback,
    {
        if let Stmt::Block { stmts } = stmt {
            cb.on_visit_block(stmt, self);

            self.scope_stack.push_scope();
            for stmt in stmts {
                self.visit_stmt(stmt, cb)?;
            }
            self.scope_stack.pop_scope();
            cb.on_exit_block(stmt, self);
        } else {
            self.visit_stmt(stmt, cb)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod scope_tests {
    use crate::parser::ast::*;

    #[test]
    fn test_block() {
        let block = Stmt::Block {
            stmts: vec![Stmt::VarDeclare {
                type_: Type::Int,
                name: "x".to_string(),
                value: Expr::Int(1),
            }],
        };

        let mut scope_pass = super::ScopePass {
            scope_stack: super::ScopeStack::new(),
        };

        let printer = &mut super::ScopePrinter { indent_level: 0 };

        scope_pass.visit_block(&block, printer).unwrap();

        // x should not be in scope after the block
        assert_eq!(scope_pass.scope_stack.resolve_symbol("x"), None)
    }

    #[test]
    fn test_nested_block() {
        let nested_block = Stmt::Block {
            stmts: vec![
                Stmt::VarDeclare {
                    type_: Type::Int,
                    name: "outer".to_string(),
                    value: Expr::Int(1),
                },
                Stmt::Block {
                    stmts: vec![
                        Stmt::VarDeclare {
                            type_: Type::Int,
                            name: "inner_x".to_string(),
                            value: Expr::Int(1),
                        },
                        Stmt::Assign {
                            name: "inner_x".to_string(),
                            value: Expr::Int(2),
                        },
                    ],
                },
            ],
        };

        let mut scope_pass = super::ScopePass {
            scope_stack: super::ScopeStack::new(),
        };

        let printer = &mut super::ScopePrinter { indent_level: 0 };

        scope_pass.visit_block(&nested_block, printer).unwrap();

        // outer should not be in scope after the block
        assert_eq!(scope_pass.scope_stack.resolve_symbol("outer"), None)
    }
}
