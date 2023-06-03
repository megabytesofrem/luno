use std::{fmt, rc::Rc};

use crate::parser::ast::{FunctionID, Type};

/// A primitive object in the interpreter.
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Array(Vec<Object>),
    Table(Vec<(String, Object)>),
    Call(FunctionID, Vec<Type>),

    // A unit type similar to `void` in C
    Unit,
}

pub struct BuiltinFunction {
    pub params: Vec<(String, Type)>,
    pub body: Box<dyn Fn(Vec<Object>) -> Object>,
}
