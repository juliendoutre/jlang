use jlang::interpreter::Interpreter;
use jlang::lexer::Lexer;
use jlang::parser::Parser;
use jlang::pretty_print::PrettyPrinter;
use jlang::type_checker::TypeChecker;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source_file> [--print-ast] [--no-type-check]", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let print_ast = args.contains(&"--print-ast".to_string());
    let no_type_check = args.contains(&"--no-type-check".to_string());

    let source = fs::read_to_string(filename).expect("Failed to read source file");

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new_with_source(lexer, source.clone());

    match parser.parse() {
        Ok(program) => {
            println!(
                "✓ Successfully parsed {} statements!",
                program.statements.len()
            );

            // Type checking (unless disabled)
            if !no_type_check {
                let mut type_checker = TypeChecker::new();
                match type_checker.check_program(&program) {
                    Ok(()) => {
                        println!("✓ Type checking passed!");
                    }
                    Err(errors) => {
                        eprintln!("\n\x1b[1;31mType checking failed with {} error(s):\x1b[0m\n", errors.len());
                        for (i, err) in errors.iter().enumerate() {
                            eprintln!("Error {}: {}", i + 1, err);
                        }
                        std::process::exit(1);
                    }
                }
            }

            if print_ast {
                println!("\n=== Abstract Syntax Tree ===\n");
                let mut printer = PrettyPrinter::new();
                printer.print_program(&program);
            } else {
                println!("\n=== Executing Program ===\n");

                let mut interpreter = Interpreter::new();
                match interpreter.execute(&program) {
                    Ok(()) => {
                        println!("\n✓ Program executed successfully!");
                    }
                    Err(err) => {
                        eprintln!("\n{}", err);
                        std::process::exit(1);
                    }
                }
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
