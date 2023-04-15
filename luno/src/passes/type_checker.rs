///! The type checker pass
use crate::{
    ast::{Block, Expr, Stmt, Type, AST},
    error::SyntaxError,
};

use super::scope::Scope;

pub struct TypeChecker {
    /// The current scope
    pub scope: Scope,

    /// The stack of scopes. The top of the stack is the current scope
    /// and we pop scopes off the stack when we exit a block
    pub scope_stack: Vec<Scope>,
}

impl TypeChecker {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let scope = Scope::new();
        Self {
            scope: scope.clone(),
            scope_stack: vec![scope],
        }
    }

    fn lookup(&self, name: &str) -> Option<Type> {
        self.scope_stack.iter().rev().find_map(|scope| {
            scope
                .symbols
                .iter()
                .find_map(|(n, t)| if n == name { Some(t.clone()) } else { None })
        })
    }

    fn insert_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope);
    }

    fn enter_scope(&mut self) {
        let scope = Scope::new();
        self.scope_stack.push(scope);

        // Update the current scope
        self.scope = self.scope_stack.last().unwrap().clone();
    }

    fn exit_scope(&mut self) {
        self.scope_stack.pop();

        // Update the current scope
        self.scope = self.scope_stack.last().unwrap().clone();
    }

    // Checking functions

    fn infer(&mut self, expr: &Expr) -> Result<Type, SyntaxError> {
        match expr {
            Expr::Int(_) => Ok(Type::Int),
            Expr::Float(_) => Ok(Type::Float),
            Expr::String(_) => Ok(Type::String),
            Expr::Boolean(_) => Ok(Type::Bool),
            Expr::Ident(name) => self
                .lookup(name)
                .ok_or(SyntaxError::NotInScope { name: name.clone() }),
            Expr::Array(xs) => {
                let mut types = vec![];

                for x in xs {
                    types.push(self.infer(x)?);
                }

                if types.iter().all(|t| t == types.first().unwrap()) {
                    Ok(Type::Array(Box::new(types.first().unwrap().clone())))
                } else {
                    let first_type = types.first().unwrap();
                    let second_type = types.get(1).unwrap();
                    Err(SyntaxError::TypeMismatch {
                        expected: Type::Array(Box::new(first_type.clone())),
                        found: Type::Array(Box::new(second_type.clone())),
                    })
                }
            }
            Expr::Call(name, args) => {
                let type_ = self.lookup(name).ok_or(SyntaxError::NotInScope {
                    name: name.to_string(),
                })?;

                match type_ {
                    Type::Function(arg_types, ret_type) => {
                        if arg_types.len() != args.len() {
                            return Err(SyntaxError::WrongNumberOfArgs {
                                expected: arg_types.len(),
                                found: args.len(),
                            });
                        }

                        for (arg, arg_type) in args.iter().zip(arg_types) {
                            let synth_type = self.infer(arg)?;
                            if arg_type != synth_type {
                                return Err(SyntaxError::TypeMismatch {
                                    expected: arg_type.clone(),
                                    found: arg_type,
                                });
                            }
                        }

                        Ok(*ret_type)
                    }
                    _ => Err(SyntaxError::NotAFunction {
                        name: name.to_string(),
                    }),
                }
            }
            _ => todo!(),
        }
    }

    fn visit_expr(&mut self, expr: &Expr, type_: &Type) -> Result<Type, SyntaxError> {
        let (exp, t) = (expr, type_);
        let synth_type = self.infer(exp)?;

        if synth_type != *t {
            Err(SyntaxError::TypeMismatch {
                expected: t.clone(),
                found: synth_type,
            })
        } else {
            Ok(synth_type)
        }
    }

    fn visit_block(&mut self, block: &Block) -> Result<(), SyntaxError> {
        self.enter_scope();

        for stmt in block.stmts.iter() {
            self.visit_stmt(stmt)?;
        }

        self.exit_scope();
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), SyntaxError> {
        match stmt {
            Stmt::VarDeclare { type_, name, value } => {
                if *type_ == Type::Unspecified {
                    // Infer the type
                    let synth_type = self.infer(value)?;
                    self.scope.symbols.insert(name.clone(), synth_type.clone());
                    self.visit_expr(value, &synth_type)?;
                } else {
                    self.scope.symbols.insert(name.clone(), type_.clone());
                    self.visit_expr(value, type_)?;
                }
            }
            Stmt::Assign { name, value } => {
                let type_ = self
                    .lookup(name)
                    .ok_or(SyntaxError::NotInScope { name: name.clone() })?;

                self.visit_expr(value, &type_)?;
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                self.visit_expr(cond, &Type::Bool)?;
                self.visit_block(then_block)?;

                if let Some(else_block) = else_block {
                    self.visit_block(else_block)?;
                }
            }
            Stmt::For {
                name: _,
                range,
                block,
            } => {
                let range_type = self.infer(range)?;
                self.visit_expr(range, &range_type)?;
                self.visit_block(block)?;
            }
            Stmt::While { cond, block } => {
                self.visit_expr(cond, &Type::Bool)?;
                self.visit_block(block)?;
            }
            Stmt::Expr { expr } => {
                let type_ = self.infer(expr)?;
                self.visit_expr(expr, &type_)?;
            }
            Stmt::Return { value } => {
                // TODO: check if we are in a function and return the correct type
            }
            _ => todo!(),
        }

        Ok(())
    }

    pub fn visit_ast(&mut self, program: &AST) -> Result<(), SyntaxError> {
        program
            .stmts
            .iter()
            .try_for_each(|stmt| self.visit_stmt(stmt))?;

        Ok(())
    }
}
