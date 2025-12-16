use jlang::interpreter::Interpreter;
use jlang::lexer::Lexer;
use jlang::parser::Parser;
use jlang::pretty_print::PrettyPrinter;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source_file> [--print-ast]", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let print_ast = args.len() > 2 && args[2] == "--print-ast";

    let source = fs::read_to_string(filename).expect("Failed to read source file");

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(program) => {
            println!(
                "✓ Successfully parsed {} statements!",
                program.statements.len()
            );

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
                        eprintln!("\n✗ Runtime error: {}", err.message);
                        std::process::exit(1);
                    }
                }
            }
        }
        Err(err) => {
            eprintln!("✗ Parse error: {}", err.message);
            std::process::exit(1);
        }
    }
}
