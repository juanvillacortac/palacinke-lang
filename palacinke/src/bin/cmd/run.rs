use std::fs;

use palacinke::utils::*;
use pk_compiler::{
    modules_table::ModulesTable,
    symbols_table::{ConstantsPool, SymbolTable},
    Compiler,
};
use pk_parser::{ast::Module, Parser};
use pk_vm::{new_globals_store, VM};

pub fn parse_file(path: &str) -> Option<Module> {
    let buff = match fs::read(path) {
        Ok(buff) => buff,
        Err(err) => {
            println!("{}", err);
            return None;
        }
    };
    let source = String::from_utf8_lossy(&buff);
    let mut parser = Parser::from_source(&source);
    match parser.parse() {
        Ok(module) => {
            return Some(module);
        }
        Err(errors) => {
            print_parse_errors(errors);
            return None;
        }
    }
}

fn eval(module: Module, path: &str) {
    let mut symbols_table = SymbolTable::new();
    let mut constants = ConstantsPool::new();
    let mut modules_table = ModulesTable::new();
    let mut compiler = Compiler::new(&mut symbols_table, &mut constants, &mut modules_table, path);
    let bytecode = match compiler.compile(module) {
        Ok(bytecode) => bytecode,
        Err(err) => {
            print_compilation_error(err);
            return;
        }
    };

    let mut globals = new_globals_store();
    let mut modules = new_globals_store();
    let mut vm = VM::new_state(&bytecode, modules_table, &mut globals, &mut modules);
    match vm.eval() {
        Ok(_) => {}
        Err(err) => print_vm_error(err),
    }
}

pub fn run(path: &str) {
    match parse_file(path) {
        Some(module) => eval(module, path),
        _ => {}
    }
}
