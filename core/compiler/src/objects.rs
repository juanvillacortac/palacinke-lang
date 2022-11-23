use std::{
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
};

use pk_parser::ast::*;

pub type BuiltinFunc = fn(Vec<Object>) -> Object;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Number(Float),
    String(String),
    Boolean(bool),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Func(Vec<Ident>, BlockStatement),
    Builtin(i32, BuiltinFunc),
    ReturnValue(Box<Object>),
    Error(String),
    Nil,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Number(Float(value)) => {
                write!(f, "{}", value)
            }
            Object::String(string) => {
                write!(f, r#""{}""#, string)
            }
            Object::Boolean(boolean) => {
                write!(f, "{}", boolean)
            }
            Object::Array(arr) => {
                write!(
                    f,
                    "[{}]",
                    arr.iter()
                        .map(|obj| format!("{}", obj))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Object::Hash(hash) => {
                write!(
                    f,
                    "{{ {} }}",
                    hash.iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Object::Func(..) => {
                write!(f, "[Function]")
            }
            Object::Builtin(..) => {
                write!(f, "[Builtin]")
            }
            Object::ReturnValue(obj) => {
                write!(f, "{}", obj)
            }
            Object::Error(error) => {
                write!(f, "[Error: {}]", error)
            }
            Object::Nil => {
                write!(f, "nil")
            }
        }
    }
}

impl Object {
    pub fn type_str(&self) -> String {
        match self {
            Object::Number(_) => "Number".to_string(),
            Object::String(_) => "String".to_string(),
            Object::Boolean(_) => "Boolean".to_string(),
            Object::Array(_) => "Array".to_string(),
            Object::Hash(_) => "Hash".to_string(),
            Object::Func(..) | Object::Builtin(..) => "Func".to_string(),
            Object::ReturnValue(_) => "Return".to_string(),
            Object::Error(_) => "Error".to_string(),
            Object::Nil => "Nil".to_string(),
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Number(ref n) => n.hash(state),
            Object::Boolean(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Float(pub f64);

impl Float {
    fn canonicalize(&self) -> i64 {
        (self.0 * 1024.0 * 1024.0).round() as i64
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Float) -> bool {
        self.canonicalize() == other.canonicalize()
    }
}

impl Eq for Float {}

impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.canonicalize().hash(state)
    }
}
