pub mod object;
pub mod scope;

use self::object::*;
use self::scope::Scope;
use crate::error::RuntimeError;
use crate::parser::ast::*;
use crate::visit::Visitor;

/// Tree-walk interpreter
pub struct Interpreter {
    /// The current scope
    pub curr_scope: Scope<Object>,

    pub scope_stack: Vec<Scope<Object>>,

    /// Global scope within the interpreter.
    pub global_scope: Scope<Object>,
}

#[allow(dead_code)]
#[allow(clippy::wrong_self_convention)]
impl Interpreter {
    fn resolve_var(&mut self, name: &str) -> Result<Object, RuntimeError> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(object) = scope.resolve(name) {
                return Ok(object);
            }
        }

        // If the variable is not in the current scope, check the global scope.
        if let Some(object) = self.global_scope.resolve(name) {
            return Ok(object);
        }

        Err(RuntimeError::NotInScope {
            name: name.to_string(),
        })
    }

    fn to_array(&mut self, expr: &Expr) -> Object {
        if let Expr::Array(arr) = expr {
            let mut objects = Vec::new();
            for expr in arr {
                objects.push(self.visit_expr(expr));
            }
            Object::Array(objects)
        } else {
            panic!("Not an array")
        }
    }

    fn to_table(&mut self, expr: &Expr) -> Object {
        if let Expr::Table(table) = expr {
            let mut objects = Vec::new();
            for (key, value) in table {
                objects.push((key.clone(), self.visit_expr(value)));
            }
            Object::Table(objects)
        } else {
            panic!("Not a table")
        }
    }
}

impl Visitor<Object> for Interpreter {
    fn visit(&mut self, ast: &Ast) -> Object {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Object {
        todo!()
    }

    fn visit_expr(&mut self, expr: &Expr) -> Object {
        match expr {
            Expr::Int(i) => Object::Int(*i),
            Expr::Float(fl) => Object::Float(*fl),
            Expr::String(s) => Object::String(s.clone()),
            Expr::Bool(b) => Object::Bool(*b),
            Expr::Array(_arr) => self.to_array(expr),
            Expr::Table(_table) => self.to_table(expr),
            Expr::Call(_name, _params) => {
                // TODO: function calls
                todo!()
            }
            Expr::Ident(ident) => self.global_scope.resolve(ident).unwrap(),

            // Not yet implemented.
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod interpreter_tests {

    #[test]
    fn test_var() {
        use super::*;
        let mut interpreter = Interpreter {
            curr_scope: Scope::new(),
            scope_stack: Vec::new(),
            global_scope: Scope::new(),
        };

        interpreter.global_scope.insert("x", Object::Int(10));

        let mut scope = Scope::new();
        scope.insert("y", Object::Int(20));
        scope.insert("z", Object::Int(30));
        interpreter.scope_stack.push(scope);

        let result = interpreter.resolve_var("x");
        assert_eq!(result.unwrap(), Object::Int(10));

        let result = interpreter.resolve_var("y");
        assert_eq!(result.unwrap(), Object::Int(20));

        let result = interpreter.resolve_var("z");
        assert_eq!(result.unwrap(), Object::Int(30));

        let result = interpreter.resolve_var("w");
        assert!(result.is_err());
    }

    #[test]
    fn test_call_builtin() {
        use super::*;

        // let mut interpreter = Interpreter {
        //     global_scope: Scope::new(),
        //     global_functions: Vec::new(),
        // };

        // interpreter.register_builtins();

        // let result = interpreter.call(0, vec![Object::String("Hello, world!".to_string())]);

        // assert_eq!(result.unwrap(), Object::Unit);
    }
}
