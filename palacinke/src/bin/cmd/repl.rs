use std::fs;

use colored::Colorize;
use palacinke::utils::*;
use pk_compiler::symbols_table::{ConstantsPool, SymbolTable};
use pk_compiler::Compiler;
use pk_parser::ast::Module;
use pk_parser::Parser;
use pk_vm::{new_globals_store, GlobalsStore, VM};
use platform_dirs::AppDirs;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn start() {
    let mut rl = match Editor::<()>::new() {
        Ok(rl) => rl,
        Err(err) => {
            println!("{}", err);
            return;
        }
    };
    let app_dirs = AppDirs::new(Some("palacinke"), false).unwrap();
    let history_path = app_dirs.data_dir.clone();

    let _ = fs::create_dir_all(&history_path);

    let history = history_path.join("history.txt");

    let _ = rl.load_history(history.as_path());

    let mut symbols_table = SymbolTable::new();
    let mut constants = ConstantsPool::new();
    let mut globals = new_globals_store();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let mut parser = Parser::from_source(&line.as_str());
                match parser.parse() {
                    Ok(module) => eval(module, &mut symbols_table, &mut constants, &mut globals),
                    Err(errors) => {
                        print_parse_errors(errors);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    let _ = rl.save_history(history.as_path());
}

fn eval(
    module: Module,
    symbols_table: &mut SymbolTable,
    constants: &mut ConstantsPool,
    globals: &mut GlobalsStore,
) {
    let fallback = (constants.clone(), symbols_table.clone());
    let mut compiler = Compiler::new(symbols_table, constants);
    match compiler.compile(module) {
        Ok(bytecode) => {
            let mut vm = VM::new_state(&bytecode, globals);
            match vm.eval() {
                Ok(result) => println!("{}", format!("{}", result).yellow().bold()),
                Err(err) => print_vm_error(err),
            }
        }
        Err(err) => {
            (*constants, *symbols_table) = fallback;
            print_compilation_error(err)
        }
    }
}
