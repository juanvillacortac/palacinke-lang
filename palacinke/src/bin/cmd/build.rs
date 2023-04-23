use std::fs;

use palacinke::utils::*;
use pk_compiler::{
    modules_table::ModulesTable,
    objects::Module,
    symbols_table::{ConstantsPool, SymbolTable},
    BytecodeDecompiler, Compiler,
};
use pk_parser::Parser;

pub fn build(path: &str, output: Option<String>) {
    let output = match output {
        Some(output) => output.to_string(),
        _ => {
            let val = format!("{}.b", path);
            val
        }
    };
    let buff = match fs::read(path) {
        Ok(buff) => buff,
        Err(err) => {
            println!("{}", err);
            return;
        }
    };
    let source = String::from_utf8_lossy(&buff);
    match compile(&source, path) {
        Some(module) => {
            if output.ends_with(".pks") {
                fs::write(output, BytecodeDecompiler::disassemble(&module.bytecode))
                    .expect("Unable to write file");
            } else {
                fs::write(output, &module.bytecode.instructions).expect("Unable to write file");
            }
        }
        _ => {}
    }
}

fn compile(source: &str, path: &str) -> Option<Module> {
    let mut parser = Parser::from_source(&source);
    let module = match parser.parse() {
        Ok(module) => module,
        Err(errors) => {
            print_parse_errors(errors);
            return None;
        }
    };
    let mut modules_table = ModulesTable::new();
    let mut symbols_table = SymbolTable::new();
    let mut constants = ConstantsPool::new();
    let mut compiler = Compiler::new(&mut symbols_table, &mut constants, &mut modules_table, path);
    let bytecode = match compiler.compile(module) {
        Ok(bytecode) => bytecode,
        Err(err) => {
            print_compilation_error(err);
            return None;
        }
    };
    Some(bytecode)
}
