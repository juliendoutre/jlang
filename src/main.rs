use clap::{Parser, Subcommand};
use jlang::formatter::Formatter;
use jlang::interpreter::Interpreter;
use jlang::lexer::Lexer;
use jlang::parser::Parser as JlangParser;
use jlang::type_checker::TypeChecker;
use std::fs;

/// JLang - A programming language with set-based types
#[derive(Parser)]
#[command(name = "jlang")]
#[command(version = "0.1.0")]
#[command(about = "JLang compiler and tools", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a JLang program
    Run {
        /// Path to the source file
        file: String,
    },
    /// Format a JLang program
    Format {
        /// Path to the source file
        file: String,

        /// Write formatted output back to file (default: print to stdout)
        #[arg(short = 'w', long)]
        write: bool,

        /// Indent size (number of spaces)
        #[arg(short, long, default_value = "4")]
        indent: usize,

        /// Check if file is formatted correctly (exit with error if not)
        #[arg(short, long)]
        check: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file } => {
            run_program(&file);
        }
        Commands::Format {
            file,
            write,
            indent,
            check,
        } => {
            format_program(&file, write, indent, check);
        }
    }
}

fn run_program(filename: &str) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!(
                "\x1b[1;31merror:\x1b[0m Failed to read file '{}': {}",
                filename, err
            );
            std::process::exit(1);
        }
    };

    let lexer = Lexer::new(&source);
    let mut parser = JlangParser::new_with_source(lexer, source.clone());

    match parser.parse() {
        Ok(program) => {
            println!(
                "✓ Successfully parsed {} statements!",
                program.statements.len()
            );

            let mut type_checker = TypeChecker::new();
            match type_checker.check_program(&program) {
                Ok(()) => {
                    println!("✓ Type checking passed!");
                }
                Err(errors) => {
                    eprintln!(
                        "\n\x1b[1;31mType checking failed with {} error(s):\x1b[0m\n",
                        errors.len()
                    );
                    for (i, err) in errors.iter().enumerate() {
                        eprintln!("Error {}: {}", i + 1, err);
                    }
                    std::process::exit(1);
                }
            }

            let mut interpreter = Interpreter::new();
            match interpreter.execute(&program) {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("\n{}", err);
                    std::process::exit(1);
                }
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}

fn format_program(filename: &str, write_back: bool, indent_size: usize, check_only: bool) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!(
                "\x1b[1;31merror:\x1b[0m Failed to read file '{}': {}",
                filename, err
            );
            std::process::exit(1);
        }
    };

    let lexer = Lexer::new(&source);
    let mut parser = JlangParser::new_with_source(lexer, source.clone());

    match parser.parse() {
        Ok(program) => {
            let mut formatter = Formatter::new().with_indent_size(indent_size);
            let formatted = formatter.format_program(&program);

            if check_only {
                // Check mode: compare formatted with original
                if formatted.trim() == source.trim() {
                    println!("✓ File is properly formatted");
                    std::process::exit(0);
                } else {
                    eprintln!(
                        "\x1b[1;33mwarning:\x1b[0m File '{}' is not properly formatted",
                        filename
                    );
                    eprintln!("Run with --write to format the file");
                    std::process::exit(1);
                }
            } else if write_back {
                // Write mode: save formatted code back to file
                match fs::write(filename, &formatted) {
                    Ok(()) => {
                        println!("✓ Formatted '{}'", filename);
                    }
                    Err(err) => {
                        eprintln!(
                            "\x1b[1;31merror:\x1b[0m Failed to write file '{}': {}",
                            filename, err
                        );
                        std::process::exit(1);
                    }
                }
            } else {
                // Default mode: print formatted code to stdout
                print!("{}", formatted);
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
