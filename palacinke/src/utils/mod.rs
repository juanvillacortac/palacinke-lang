use colored::Colorize;
use pk_compiler::errors::CompilationError;
use pk_parser::ParseErrors;
use pk_vm::errors::VMError;

pub fn print_parse_errors(errors: ParseErrors) {
    println!(
        "{}",
        format!("{} parsing errors:", errors.len()).red().bold(),
    );
    let msg = errors
        .into_iter()
        .map(|e| format!("-> {}", e))
        .collect::<Vec<String>>()
        .join("\n");
    println!("{}", msg.white().bold());
}

pub fn print_vm_error(error: VMError) {
    println!("{}", "Evaluation error:".red().bold());
    println!("{}", format!("-> {}", error).white().bold());
}

pub fn print_compilation_error(error: CompilationError) {
    println!("{}", "Compilation error:".red().bold());
    println!("{}", format!("-> {}", error).white().bold());
}
