use std::fs;
use std::path::PathBuf;

use colored::Colorize;
use palacinke::utils::*;
use pk_compiler::modules_table::ModulesTable;
use pk_compiler::symbols_table::{ConstantsPool, SymbolTable};
use pk_compiler::{BytecodeDecompiler, Compiler};
use pk_parser::ast;
use pk_parser::Parser;
use pk_vm::{new_globals_store, GlobalsStore, ModulesStore, VM};
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

    let absolute_path = std::env::current_dir().unwrap();

    let history = history_path.join("history.txt");

    let _ = rl.load_history(history.as_path());

    let mut modules_table = ModulesTable::new();
    let mut symbols_table = SymbolTable::new();
    let mut constants = ConstantsPool::new();
    let mut globals = new_globals_store();
    let mut modules = new_globals_store();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let source = &line.as_str().strip_prefix(":ir ").unwrap_or(&line.as_str());
                let mut parser = Parser::from_source(source);
                match parser.parse() {
                    Ok(module) => {
                        if line.as_str().starts_with(":ir ") {
                            disasm(
                                module,
                                &mut symbols_table,
                                &mut constants,
                                &mut modules_table,
                                &absolute_path,
                            );
                        } else {
                            eval(
                                module,
                                &mut symbols_table,
                                &mut constants,
                                &mut modules_table,
                                &mut globals,
                                &mut modules,
                                &absolute_path,
                            )
                        }
                    }
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

fn disasm(
    module: ast::Module,
    symbols_table: &mut SymbolTable,
    constants: &mut ConstantsPool,
    modules_table: &mut ModulesTable,
    path: &PathBuf,
) {
    let fallback = (
        constants.clone(),
        symbols_table.clone(),
        modules_table.clone(),
    );
    let mut compiler = Compiler::new(
        symbols_table,
        constants,
        modules_table,
        path.to_str().unwrap(),
    );
    match compiler.compile(module) {
        Ok(module) => {
            println!("{}", BytecodeDecompiler::disassemble(&module.bytecode));
        }
        Err(err) => print_compilation_error(err),
    }
    (*constants, *symbols_table, *modules_table) = fallback;
}

fn eval(
    module: ast::Module,
    symbols_table: &mut SymbolTable,
    constants: &mut ConstantsPool,
    modules_table: &mut ModulesTable,
    globals: &mut GlobalsStore,
    modules: &mut ModulesStore,
    path: &PathBuf,
) {
    let fallback = (
        constants.clone(),
        symbols_table.clone(),
        modules_table.clone(),
    );
    let mut compiler = Compiler::new(
        symbols_table,
        constants,
        modules_table,
        path.to_str().unwrap(),
    );
    match compiler.compile(module) {
        Ok(bytecode) => {
            let mut vm = VM::new_state(&bytecode, modules_table.clone(), globals, modules);
            match vm.eval() {
                Ok(result) => println!("{}", format!("{}", result).yellow().bold()),
                Err(err) => print_vm_error(err),
            }
        }
        Err(err) => {
            (*constants, *symbols_table, *modules_table) = fallback;
            print_compilation_error(err)
        }
    }
}
