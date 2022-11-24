use crate::errors::*;
use pk_compiler::{objects::Object, Compiler};
use pk_parser::{ast::Module, Parser};
use pk_vm::VM;

pub mod errors;
pub mod utils;

pub fn eval_from_buff(source: &str) -> Result<Option<Object>, EvaluationError> {
    let mut parser = Parser::from_source(&source);
    match parser.parse() {
        Ok(module) => eval(module),
        Err(err) => Err(EvaluationError::ParseErrors(err)),
    }
}

pub fn eval(module: Module) -> Result<Option<Object>, EvaluationError> {
    let mut compiler = Compiler::new();
    let bytecode = match compiler.compile(module) {
        Ok(bytecode) => bytecode,
        Err(err) => {
            return Err(EvaluationError::CompilationError(err));
        }
    };

    let mut vm = VM::new(bytecode);
    match vm.run() {
        Some(err) => Err(EvaluationError::VMError(err)),
        _ => Ok(vm.last_popped()),
    }
}
