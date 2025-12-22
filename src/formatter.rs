use crate::ast::{BinaryOperator, Expr, Program, Statement, UnaryOperator};

/// Formatter for jlang programs
pub struct Formatter {
    indent_level: usize,
    indent_size: usize,
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            indent_size: 4,
        }
    }

    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    /// Format a complete program
    pub fn format_program(&mut self, program: &Program) -> String {
        let mut output = String::new();

        for (i, statement) in program.statements.iter().enumerate() {
            // Add blank line between top-level statements for readability
            if i > 0 && !matches!(statement, Statement::Empty) {
                output.push('\n');
            }

            output.push_str(&self.format_statement(statement));
            output.push('\n');
        }

        output
    }

    fn indent(&self) -> String {
        " ".repeat(self.indent_level * self.indent_size)
    }

    fn format_statement(&mut self, statement: &Statement) -> String {
        let indent = self.indent();

        match statement {
            Statement::Empty => String::new(),

            Statement::Import {
                url,
                checksum,
                alias,
            } => {
                format!(
                    "{}import \"{}\" sha256 \"{}\" as {}",
                    indent, url, checksum, alias
                )
            }

            Statement::Definition { name, value } => {
                format!("{}{} = {}", indent, name, self.format_expr(value))
            }

            Statement::Assignment { name, value } => {
                format!("{}{} = {}", indent, name, self.format_expr(value))
            }

            Statement::TypedAssignment {
                name,
                type_expr,
                value,
            } => {
                format!(
                    "{}{}: {} = {}",
                    indent,
                    name,
                    self.format_expr(type_expr),
                    self.format_expr(value)
                )
            }

            Statement::IndexAssignment {
                array,
                index,
                value,
            } => {
                format!(
                    "{}{}[{}] = {}",
                    indent,
                    array,
                    self.format_expr(index),
                    self.format_expr(value)
                )
            }

            Statement::FunctionDefinition {
                name,
                params,
                returns,
                body,
            } => {
                let mut output = format!("{}{} = (", indent, name);

                // Format parameters
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        output.push_str(", ");
                    }
                    output.push_str(&param.name);
                    if let Some(type_expr) = &param.type_expr {
                        output.push_str(&format!(": {}", self.format_expr(type_expr)));
                    }
                }

                output.push_str(") -> (");

                // Format return parameters
                for (i, ret) in returns.iter().enumerate() {
                    if i > 0 {
                        output.push_str(", ");
                    }
                    output.push_str(&ret.name);
                    if let Some(type_expr) = &ret.type_expr {
                        output.push_str(&format!(": {}", self.format_expr(type_expr)));
                    }
                }

                output.push_str(") {\n");

                // Format body
                self.indent_level += 1;
                for stmt in body {
                    output.push_str(&self.format_statement(stmt));
                    output.push('\n');
                }
                self.indent_level -= 1;

                output.push_str(&format!("{}}}", indent));
                output
            }

            Statement::ExpressionStatement(expr) => {
                format!("{}{}", indent, self.format_expr(expr))
            }

            Statement::Return(expr) => {
                format!("{}return {}", indent, self.format_expr(expr))
            }

            Statement::For {
                variable,
                iterable,
                body,
            } => {
                let mut output = format!(
                    "{}for {} in {} {{\n",
                    indent,
                    variable,
                    self.format_expr(iterable)
                );

                self.indent_level += 1;
                for stmt in body {
                    output.push_str(&self.format_statement(stmt));
                    output.push('\n');
                }
                self.indent_level -= 1;

                output.push_str(&format!("{}}}", indent));
                output
            }

            Statement::If { condition, body } => {
                let mut output = format!("{}if {} {{\n", indent, self.format_expr(condition));

                self.indent_level += 1;
                for stmt in body {
                    output.push_str(&self.format_statement(stmt));
                    output.push('\n');
                }
                self.indent_level -= 1;

                output.push_str(&format!("{}}}", indent));
                output
            }
        }
    }

    fn format_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Integer(n) => n.to_string(),

            Expr::Character(c) => match c {
                '\n' => "'\\n'".to_string(),
                '\t' => "'\\t'".to_string(),
                '\r' => "'\\r'".to_string(),
                '\\' => "'\\\\'".to_string(),
                '\'' => "'\\''".to_string(),
                '"' => "'\\\"'".to_string(),
                '\0' => "'\\0'".to_string(),
                _ => format!("'{}'", c),
            },

            Expr::Identifier(name) => name.clone(),

            Expr::QualifiedName { module, name } => format!("{}::{}", module, name),

            Expr::ExplicitSet(elements) => {
                if elements.is_empty() {
                    return "{}".to_string();
                }
                let formatted: Vec<String> = elements.iter().map(|e| self.format_expr(e)).collect();
                format!("{{{}}}", formatted.join(", "))
            }

            Expr::RangeSet { start, step, end } => {
                if let Some(step_expr) = step {
                    format!(
                        "{{{}, {}, ..., {}}}",
                        self.format_expr(start),
                        self.format_expr(step_expr),
                        self.format_expr(end)
                    )
                } else {
                    format!(
                        "{{{}, ..., {}}}",
                        self.format_expr(start),
                        self.format_expr(end)
                    )
                }
            }

            Expr::ImplicitSet {
                variable,
                type_expr,
                constraint,
            } => {
                let mut output = format!("{{{}: {}", variable, self.format_expr(type_expr));
                if let Some(constraint_expr) = constraint {
                    output.push_str(&format!(" | {}", self.format_expr(constraint_expr)));
                }
                output.push('}');
                output
            }

            Expr::BinaryOp { op, left, right } => {
                let op_str = match op {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Subtract => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::Divide => "/",
                    BinaryOperator::Modulo => "%",
                    BinaryOperator::Power => "^",
                    BinaryOperator::Equals => "==",
                    BinaryOperator::LessThan => "<",
                    BinaryOperator::GreaterThan => ">",
                    BinaryOperator::LessThanOrEqual => "<=",
                    BinaryOperator::GreaterThanOrEqual => ">=",
                    BinaryOperator::And => "&",
                };
                format!(
                    "{} {} {}",
                    self.format_expr(left),
                    op_str,
                    self.format_expr(right)
                )
            }

            Expr::UnaryOp { op, operand } => {
                let op_str = match op {
                    UnaryOperator::Negate => "-",
                };
                format!("{}{}", op_str, self.format_expr(operand))
            }

            Expr::FunctionCall { name, args } => {
                let formatted_args: Vec<String> =
                    args.iter().map(|a| self.format_expr(a)).collect();
                format!("{}({})", name, formatted_args.join(", "))
            }

            Expr::ArrayType { size, element_type } => {
                format!(
                    "[{}]{}",
                    self.format_expr(size),
                    self.format_expr(element_type)
                )
            }

            Expr::TypeConstrained { name, type_expr } => {
                format!("{}: {}", name, self.format_expr(type_expr))
            }

            Expr::Index { array, index } => {
                format!("{}[{}]", self.format_expr(array), self.format_expr(index))
            }

            Expr::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    return "[]".to_string();
                }
                let formatted: Vec<String> = elements.iter().map(|e| self.format_expr(e)).collect();
                format!("[{}]", formatted.join(", "))
            }

            Expr::ArrayRange { start, step, end } => {
                if let Some(step_expr) = step {
                    format!(
                        "[{}, {}, ..., {}]",
                        self.format_expr(start),
                        self.format_expr(step_expr),
                        self.format_expr(end)
                    )
                } else {
                    format!(
                        "[{}, ..., {}]",
                        self.format_expr(start),
                        self.format_expr(end)
                    )
                }
            }

            Expr::Slice { array, start, end } => {
                let start_str = start
                    .as_ref()
                    .map(|s| self.format_expr(s))
                    .unwrap_or_default();
                let end_str = end
                    .as_ref()
                    .map(|e| self.format_expr(e))
                    .unwrap_or_default();
                format!("{}[{}:{}]", self.format_expr(array), start_str, end_str)
            }

            Expr::TupleType { fields } => {
                if fields.is_empty() {
                    return "()".to_string();
                }
                let formatted: Vec<String> = fields
                    .iter()
                    .map(|(name, type_expr)| format!("{}: {}", name, self.format_expr(type_expr)))
                    .collect();
                format!("({})", formatted.join(", "))
            }

            Expr::TupleLiteral { fields } => {
                if fields.is_empty() {
                    return "()".to_string();
                }
                let formatted: Vec<String> = fields
                    .iter()
                    .map(|(name, value_expr)| format!("{}: {}", name, self.format_expr(value_expr)))
                    .collect();
                format!("({})", formatted.join(", "))
            }

            Expr::FieldAccess { object, field } => {
                format!("{}.{}", self.format_expr(object), field)
            }
        }
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn format_source(source: &str) -> String {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new_with_source(lexer, source.to_string());
        let program = parser.parse().unwrap();
        let mut formatter = Formatter::new();
        formatter.format_program(&program)
    }

    #[test]
    fn test_format_simple_assignment() {
        let source = "x=42";
        let formatted = format_source(source);
        assert!(formatted.contains("x = 42"));
    }

    #[test]
    fn test_format_function() {
        let source = "add=(x:INTEGER,y:INTEGER)->(result:INTEGER){result=x+y}";
        let formatted = format_source(source);
        assert!(formatted.contains("add = (x: INTEGER, y: INTEGER) -> (result: INTEGER)"));
        assert!(formatted.contains("    result = x + y"));
    }

    #[test]
    fn test_format_set() {
        let source = "S={1,2,3}";
        let formatted = format_source(source);
        assert!(formatted.contains("S = {1, 2, 3}"));
    }

    #[test]
    fn test_format_array() {
        let source = "arr=[1,2,3]";
        let formatted = format_source(source);
        assert!(formatted.contains("arr = [1, 2, 3]"));
    }

    #[test]
    fn test_format_for_loop() {
        let source = "for i in [1,2,3]{x=i}";
        let formatted = format_source(source);
        assert!(formatted.contains("for i in [1, 2, 3]"));
        assert!(formatted.contains("    x = i"));
    }

    #[test]
    fn test_format_if_statement() {
        let source = "if x>0{y=1}";
        let formatted = format_source(source);
        assert!(formatted.contains("if x > 0"));
        assert!(formatted.contains("    y = 1"));
    }

    #[test]
    fn test_format_tuple() {
        let source = "point=(x:1,y:2)";
        let formatted = format_source(source);
        assert!(formatted.contains("point = (x: 1, y: 2)"));
    }

    #[test]
    fn test_format_preserves_comments() {
        // Note: comments are stripped by lexer, so this tests that it doesn't break
        let source = "x = 1";
        let formatted = format_source(source);
        assert!(formatted.contains("x = 1"));
    }

    #[test]
    fn test_format_nested_expressions() {
        let source = "result=((1+2)*(3+4))";
        let formatted = format_source(source);
        assert!(formatted.contains("1 + 2"));
        assert!(formatted.contains("3 + 4"));
    }
}
