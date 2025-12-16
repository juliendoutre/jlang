use crate::ast::{Expr, Program, Statement};

pub struct PrettyPrinter {
    indent: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self { indent: 0 }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.indent)
    }

    pub fn print_program(&mut self, program: &Program) {
        for (i, stmt) in program.statements.iter().enumerate() {
            println!("{}Statement {}:", self.indent(), i + 1);
            self.indent += 1;
            self.print_statement(stmt);
            self.indent -= 1;
            println!();
        }
    }

    fn print_statement(&self, stmt: &Statement) {
        match stmt {
            Statement::Definition { name, value } => {
                println!("{}Type Definition: {} = ", self.indent(), name);
                self.print_expr(value, self.indent + 1);
            }
            Statement::Assignment { name, value } => {
                println!("{}Variable Assignment: {} = ", self.indent(), name);
                self.print_expr(value, self.indent + 1);
            }
            Statement::ExpressionStatement(expr) => {
                println!("{}Expression Statement:", self.indent());
                self.print_expr(expr, self.indent + 1);
            }
            Statement::Empty => {
                println!("{}Empty", self.indent());
            }
        }
    }

    fn print_expr(&self, expr: &Expr, indent: usize) {
        let indent_str = "  ".repeat(indent);
        match expr {
            Expr::Identifier(name) => {
                println!("{}Identifier({})", indent_str, name);
            }
            Expr::Integer(n) => {
                println!("{}Integer({})", indent_str, n);
            }
            Expr::ExplicitSet(elements) => {
                if elements.is_empty() {
                    println!("{}Empty Set {{}}", indent_str);
                } else {
                    println!("{}Explicit Set {{", indent_str);
                    for elem in elements {
                        self.print_expr(elem, indent + 1);
                    }
                    println!("{}}}", indent_str);
                }
            }
            Expr::RangeSet { start, step, end } => {
                println!("{}Range Set {{", indent_str);
                print!("{}  start: ", indent_str);
                self.print_expr(start, 0);
                if let Some(step) = step {
                    print!("{}  step: ", indent_str);
                    self.print_expr(step, 0);
                } else {
                    println!("{}  step: implicit (1)", indent_str);
                }
                print!("{}  end: ", indent_str);
                self.print_expr(end, 0);
                println!("{}}}", indent_str);
            }
            Expr::ImplicitSet {
                variable,
                type_expr,
            } => {
                println!("{}Implicit Set {{ {}: ", indent_str, variable);
                self.print_expr(type_expr, indent + 1);
                println!("{}}}", indent_str);
            }
            Expr::ConstrainedSet {
                variable,
                type_expr,
                constraint,
            } => {
                println!("{}Constrained Set {{ {}: ", indent_str, variable);
                self.print_expr(type_expr, indent + 1);
                println!("{}  |", indent_str);
                self.print_expr(constraint, indent + 1);
                println!("{}}}", indent_str);
            }
            Expr::BinaryOp { op, left, right } => {
                println!("{}Binary Op: {:?}", indent_str, op);
                print!("{}  left: ", indent_str);
                self.print_expr(left, 0);
                print!("{}  right: ", indent_str);
                self.print_expr(right, 0);
            }
            Expr::FunctionCall { name, args } => {
                if args.is_empty() {
                    println!("{}Function Call: {}()", indent_str, name);
                } else {
                    println!("{}Function Call: {}(", indent_str, name);
                    for arg in args {
                        self.print_expr(arg, indent + 1);
                    }
                    println!("{})", indent_str);
                }
            }
        }
    }
}
