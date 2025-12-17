use crate::ast::{Expr, Program, Statement, UnaryOperator};

pub struct PrettyPrinter {
    indent: usize,
}

impl Default for PrettyPrinter {
    fn default() -> Self {
        Self::new()
    }
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
            Statement::IndexAssignment {
                array,
                index,
                value,
            } => {
                println!("{}Index Assignment: {}[", self.indent(), array);
                self.print_expr(index, self.indent + 1);
                println!("{}] = ", self.indent());
                self.print_expr(value, self.indent + 1);
            }
            Statement::TypedAssignment {
                name,
                type_expr,
                value,
            } => {
                println!("{}Typed Assignment: {}: ", self.indent(), name);
                self.print_expr(type_expr, self.indent + 1);
                println!("{}  = ", self.indent());
                self.print_expr(value, self.indent + 1);
            }
            Statement::FunctionDefinition {
                name,
                params,
                returns,
                body,
            } => {
                println!("{}Function Definition: {}", self.indent(), name);
                println!("{}  Parameters:", self.indent());
                for param in params {
                    if let Some(type_expr) = &param.type_expr {
                        print!("{}    {}: ", self.indent(), param.name);
                        self.print_expr(type_expr, 0);
                    } else {
                        println!("{}    {}", self.indent(), param.name);
                    }
                }
                println!("{}  Returns:", self.indent());
                for ret in returns {
                    if let Some(type_expr) = &ret.type_expr {
                        print!("{}    {}: ", self.indent(), ret.name);
                        self.print_expr(type_expr, 0);
                    } else {
                        println!("{}    {}", self.indent(), ret.name);
                    }
                }
                println!("{}  Body: {} statements", self.indent(), body.len());
            }
            Statement::ExpressionStatement(expr) => {
                println!("{}Expression Statement:", self.indent());
                self.print_expr(expr, self.indent + 1);
            }
            Statement::Return(expr) => {
                println!("{}Return:", self.indent());
                self.print_expr(expr, self.indent + 1);
            }
            Statement::For {
                variable,
                iterable,
                body,
            } => {
                println!("{}For Loop:", self.indent());
                println!("{}  variable: {}", self.indent(), variable);
                println!("{}  iterable:", self.indent());
                self.print_expr(iterable, self.indent + 2);
                println!("{}  body: {} statements", self.indent(), body.len());
            }
            Statement::If { condition, body } => {
                println!("{}If Statement:", self.indent());
                println!("{}  condition:", self.indent());
                self.print_expr(condition, self.indent + 2);
                println!("{}  body: {} statements", self.indent(), body.len());
            }
            Statement::Empty => {
                println!("{}Empty", self.indent());
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn print_expr(&self, expr: &Expr, indent: usize) {
        let indent_str = "  ".repeat(indent);
        match expr {
            Expr::Identifier(name) => {
                println!("{}Identifier({})", indent_str, name);
            }
            Expr::Integer(n) => {
                println!("{}Integer({})", indent_str, n);
            }
            Expr::Character(c) => {
                println!("{}Character('{}') [UTF-8: {}]", indent_str, c, *c as u32);
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
                constraint,
            } => {
                println!("{}Implicit Set {{ {}: ", indent_str, variable);
                self.print_expr(type_expr, indent + 1);
                if let Some(constraint) = constraint {
                    println!("{}  |", indent_str);
                    self.print_expr(constraint, indent + 1);
                }
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
            Expr::ArrayType { size, element_type } => {
                println!("{}Array Type [", indent_str);
                print!("{}  size: ", indent_str);
                self.print_expr(size, 0);
                print!("{}  element: ", indent_str);
                self.print_expr(element_type, 0);
                println!("{}]", indent_str);
            }
            Expr::TypeConstrained { name, type_expr } => {
                print!("{}{}: ", indent_str, name);
                self.print_expr(type_expr, 0);
            }
            Expr::Index { array, index } => {
                println!("{}Array Index:", indent_str);
                print!("{}  array: ", indent_str);
                self.print_expr(array, 0);
                print!("{}  index: ", indent_str);
                self.print_expr(index, 0);
            }
            Expr::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    println!("{}Array Literal []", indent_str);
                } else {
                    println!("{}Array Literal [", indent_str);
                    for elem in elements {
                        self.print_expr(elem, indent + 1);
                    }
                    println!("{}]", indent_str);
                }
            }
            Expr::ArrayRange { start, step, end } => {
                println!("{}Array Range [", indent_str);
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
                println!("{}]", indent_str);
            }
            Expr::Slice { array, start, end } => {
                println!("{}Array Slice:", indent_str);
                print!("{}  array: ", indent_str);
                self.print_expr(array, 0);
                if let Some(start_expr) = start {
                    print!("{}  start: ", indent_str);
                    self.print_expr(start_expr, 0);
                } else {
                    println!("{}  start: implicit (0)", indent_str);
                }
                if let Some(end_expr) = end {
                    print!("{}  end: ", indent_str);
                    self.print_expr(end_expr, 0);
                } else {
                    println!("{}  end: implicit (length)", indent_str);
                }
            }

            Expr::TupleType { fields } => {
                println!("{}TupleType:", indent_str);
                for (name, type_expr) in fields {
                    print!("{}  {}: ", indent_str, name);
                    self.print_expr(type_expr, 0);
                }
            }

            Expr::TupleLiteral { fields } => {
                println!("{}TupleLiteral:", indent_str);
                for (name, value_expr) in fields {
                    print!("{}  {}: ", indent_str, name);
                    self.print_expr(value_expr, 0);
                }
            }

            Expr::FieldAccess { object, field } => {
                println!("{}FieldAccess:", indent_str);
                print!("{}  object: ", indent_str);
                self.print_expr(object, 0);
                println!("{}  field: {}", indent_str, field);
            }

            Expr::UnaryOp { op, operand } => {
                let op_str = match op {
                    UnaryOperator::Negate => "-",
                };
                println!("{}UnaryOp: {}", indent_str, op_str);
                print!("{}  operand: ", indent_str);
                self.print_expr(operand, 0);
            }
        }
    }
}
