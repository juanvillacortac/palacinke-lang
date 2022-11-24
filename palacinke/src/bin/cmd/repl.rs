use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

use palacinke::utils::*;
use pk_compiler::symbols_table::SymbolTable;
use pk_compiler::{new_constants_pool, Compiler, ConstantsPool};
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

    let symbols_table = Rc::new(RefCell::new(SymbolTable::new()));
    let constants = Rc::new(RefCell::new(new_constants_pool()));
    let globals = Rc::new(RefCell::new(new_globals_store()));

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let mut parser = Parser::from_source(&line.as_str());
                match parser.parse() {
                    Ok(module) => eval(
                        module,
                        symbols_table.to_owned(),
                        constants.to_owned(),
                        globals.to_owned(),
                    ),
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
    symbols_table: Rc<RefCell<SymbolTable>>,
    constants: Rc<RefCell<ConstantsPool>>,
    globals: Rc<RefCell<GlobalsStore>>,
) {
    let mut compiler = Compiler::new_with_state(symbols_table, constants);
    match compiler.compile(module) {
        Ok(bytecode) => {
            let mut vm = VM::new_with_globals(bytecode, globals);
            match vm.run() {
                None => {
                    let stack_top = vm.last_popped();
                    if let Some(value) = stack_top {
                        println!("{}", value)
                    }
                }
                Some(err) => print_vm_error(err),
            }
        }
        Err(err) => print_compilation_error(err),
    }
}
