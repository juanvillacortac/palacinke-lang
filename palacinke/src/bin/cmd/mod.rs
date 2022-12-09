use clap::{command, Parser, Subcommand};

mod build;
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

    #[command(about = "Compile a script to bytecode", arg_required_else_help = true)]
    Build {
        #[arg(required = true)]
        path: String,
        #[arg(
            long,
            short = 'o',
            help = "if the output extension ends with \".pks\" the script will be compiled to IR instead"
        )]
        output: Option<String>,
    },

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
            Commands::Build { path, output } => {
                build::build(&path, output.clone());
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
