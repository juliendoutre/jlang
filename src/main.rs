use jlang::lexer::Lexer;
use jlang::parser::Parser;
use jlang::pretty_print::PrettyPrinter;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let source = fs::read_to_string(filename).expect("Failed to read source file");

    println!("=== Lexing ===");
    let lexer = Lexer::new(&source);
    let tokens: Vec<_> = lexer.collect();

    for token in &tokens {
        println!("{:?}", token);
    }

    println!("\n=== Parsing ===");
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(program) => {
            println!(
                "✓ Successfully parsed {} statements!\n",
                program.statements.len()
            );
            println!("=== Abstract Syntax Tree ===\n");

            let mut printer = PrettyPrinter::new();
            printer.print_program(&program);
        }
        Err(err) => {
            eprintln!("✗ Parse error: {}", err.message);
            std::process::exit(1);
        }
    }
}
