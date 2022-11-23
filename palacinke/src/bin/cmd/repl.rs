use std::fs;

use palacinke::utils::*;
use pk_compiler::Compiler;
use pk_parser::ast::Module;
use pk_parser::Parser;
use pk_vm::VM;
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
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let mut parser = Parser::from_source(&line.as_str());
                match parser.parse() {
                    Ok(module) => eval(module),
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

fn eval(module: Module) {
    let mut compiler = Compiler::new();
    match compiler.compile(module) {
        Ok(bytecode) => {
            let mut vm = VM::new(bytecode);
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
