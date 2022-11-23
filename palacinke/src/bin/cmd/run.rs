use std::fs;

use palacinke::utils::*;
use pk_compiler::Compiler;
use pk_parser::{ast::Module, Parser};
use pk_vm::VM;

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

fn eval(module: Module) {
    let mut compiler = Compiler::new();
    let bytecode = match compiler.compile(module) {
        Ok(bytecode) => bytecode,
        Err(err) => {
            print_compilation_error(err);
            return;
        }
    };

    let mut vm = VM::new(bytecode);
    match vm.run() {
        Some(err) => print_vm_error(err),
        _ => {}
    }
}

pub fn run(path: &str) {
    match parse_file(path) {
        Some(module) => eval(module),
        _ => {}
    }
}
