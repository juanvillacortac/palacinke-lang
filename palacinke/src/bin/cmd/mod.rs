use clap::{command, Parser, Subcommand};

mod repl;
mod run;

#[derive(Debug, Parser)]
#[command(name = "palacinke")]
#[command(about = "palacinke: the useless scripting lang", version = option_env!("CARGO_PKG_VERSION"))]
struct Cli {
    #[command(subcommand)]
    commands: Option<Commands>,
}

#[derive(Debug, Subcommand)]
enum Commands {
    #[command(about = "Eval a script")]
    Run { path: String },

    #[command(about = "Enter to the repl mode")]
    Repl,
}

pub fn start() {
    let args = Cli::parse();
    match &args.commands {
        Some(command) => match command {
            Commands::Run { path } => {
                run::run(&path);
            }
            Commands::Repl => {
                repl::start();
            }
        },
        _ => {
            repl::start();
        }
    };
}
