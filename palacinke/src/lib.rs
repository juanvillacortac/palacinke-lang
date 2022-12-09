use crate::errors::*;
use pk_compiler::{
    objects::Object,
    symbols_table::{ConstantsPool, SymbolTable},
    Compiler,
};
use pk_parser::{ast::Module, Parser};
use pk_vm::{new_globals_store, VM};

pub mod errors;
pub mod utils;

pub fn eval_from_buff(source: &str) -> Result<Object, EvaluationError> {
    let mut parser = Parser::from_source(&source);
    match parser.parse() {
        Ok(module) => eval(module),
        Err(err) => Err(EvaluationError::ParseErrors(err)),
    }
}

pub fn eval(module: Module) -> Result<Object, EvaluationError> {
    let mut symbols_table = SymbolTable::new();
    let mut constants = ConstantsPool::new();
    let mut compiler = Compiler::new(&mut symbols_table, &mut constants);
    let bytecode = match compiler.compile(module) {
        Ok(bytecode) => bytecode,
        Err(err) => {
            return Err(EvaluationError::CompilationError(err));
        }
    };

    let mut globals = new_globals_store();
    let mut vm = VM::new_state(&bytecode, &mut globals);
    match vm.eval() {
        Err(err) => Err(EvaluationError::VMError(err)),
        Ok(result) => Ok((*result).clone()),
    }
}
