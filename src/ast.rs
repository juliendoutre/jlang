/// Abstract Syntax Tree node types

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// An identifier (variable or type name)
    Identifier(String),

    /// An integer literal
    Integer(i64),

    /// A character literal (converted to UTF-8 value)
    Character(char),

    /// Explicit set: { 0, 1, 2, 3 }
    ExplicitSet(Vec<Expr>),

    /// Range set: { 0, 1, ..., 5 }
    RangeSet {
        start: Box<Expr>,
        step: Option<Box<Expr>>,
        end: Box<Expr>,
    },

    /// Implicit set: { x: BYTE } or { x: BYTE | x % 2 == 0 }
    ImplicitSet {
        variable: String,
        type_expr: Box<Expr>,
        constraint: Option<Box<Expr>>,
    },

    /// Binary operation
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    /// Unary operation
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expr>,
    },

    /// Function call: card(A), out(b), in()
    FunctionCall { name: String, args: Vec<Expr> },

    /// Array type: [n]BYTE or [5]BYTE or [n: BYTE]BYTE
    ArrayType {
        size: Box<Expr>,
        element_type: Box<Expr>,
    },

    /// Type-constrained expression: n: BYTE
    /// Used in array sizes and other contexts where a value has a type constraint
    TypeConstrained { name: String, type_expr: Box<Expr> },

    /// Array indexing: array[index]
    Index { array: Box<Expr>, index: Box<Expr> },

    /// Array literal: [1, 2, 3]
    ArrayLiteral(Vec<Expr>),

    /// Array range: [0, 1, ..., n]
    ArrayRange {
        start: Box<Expr>,
        step: Option<Box<Expr>>,
        end: Box<Expr>,
    },

    /// Array slice: array[start:end], array[:end], array[start:], array[:]
    Slice {
        array: Box<Expr>,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    },

    /// Tuple type: (x: INTEGER, y: INTEGER, z: INTEGER)
    TupleType { fields: Vec<(String, Expr)> },

    /// Tuple literal: (x: 3, y: -8, z: 0)
    TupleLiteral { fields: Vec<(String, Expr)> },

    /// Field access: tuple.field
    FieldAccess { object: Box<Expr>, field: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,                // +
    Subtract,           // -
    Multiply,           // *
    Divide,             // /
    Power,              // ^
    Equals,             // ==
    Modulo,             // %
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=
    And,                // &
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Negate, // -
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Type/set definition: A = { 0, 1, 2 }
    Definition { name: String, value: Expr },

    /// Variable assignment: a = card(A)
    Assignment { name: String, value: Expr },

    /// Array element assignment: arr[i] = value
    IndexAssignment {
        array: String,
        index: Expr,
        value: Expr,
    },

    /// Variable assignment with type constraint: n: BYTE = in()
    TypedAssignment {
        name: String,
        type_expr: Expr,
        value: Expr,
    },

    /// Function definition: f = (x: T) -> (y: U) { body }
    FunctionDefinition {
        name: String,
        params: Vec<Parameter>,
        returns: Vec<Parameter>,
        body: Vec<Statement>,
    },

    /// Expression statement (function call): out(b)
    ExpressionStatement(Expr),

    /// Return statement: return value
    Return(Expr),

    /// For loop: for x in array { body }
    For {
        variable: String,
        iterable: Expr,
        body: Vec<Statement>,
    },

    /// If statement: if condition { body }
    If {
        condition: Expr,
        body: Vec<Statement>,
    },

    /// Comment or empty line (we'll keep these for completeness)
    Empty,
}

/// Function parameter with optional type constraint
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub type_expr: Option<Expr>,
}

/// The top-level AST is a list of statements
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn add_statement(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program_new() {
        let program = Program::new();
        assert_eq!(program.statements.len(), 0);
    }

    #[test]
    fn test_program_default() {
        let program = Program::default();
        assert_eq!(program.statements.len(), 0);
    }

    #[test]
    fn test_program_add_statement() {
        let mut program = Program::new();
        program.add_statement(Statement::Empty);
        program.add_statement(Statement::Assignment {
            name: "x".to_string(),
            value: Expr::Integer(42),
        });

        assert_eq!(program.statements.len(), 2);
        assert_eq!(program.statements[0], Statement::Empty);
    }

    #[test]
    fn test_parameter_creation() {
        let param = Parameter {
            name: "x".to_string(),
            type_expr: Some(Expr::Identifier("INTEGER".to_string())),
        };

        assert_eq!(param.name, "x");
        assert!(param.type_expr.is_some());
    }

    #[test]
    fn test_expr_equality() {
        let expr1 = Expr::Integer(42);
        let expr2 = Expr::Integer(42);
        let expr3 = Expr::Integer(43);

        assert_eq!(expr1, expr2);
        assert_ne!(expr1, expr3);
    }

    #[test]
    fn test_binary_op_equality() {
        assert_eq!(BinaryOperator::Add, BinaryOperator::Add);
        assert_ne!(BinaryOperator::Add, BinaryOperator::Subtract);
    }

    #[test]
    fn test_unary_op_equality() {
        assert_eq!(UnaryOperator::Negate, UnaryOperator::Negate);
    }

    #[test]
    fn test_statement_equality() {
        let stmt1 = Statement::Empty;
        let stmt2 = Statement::Empty;
        let stmt3 = Statement::Assignment {
            name: "x".to_string(),
            value: Expr::Integer(1),
        };

        assert_eq!(stmt1, stmt2);
        assert_ne!(stmt1, stmt3);
    }

    #[test]
    fn test_expr_clone() {
        let expr1 = Expr::BinaryOp {
            op: BinaryOperator::Add,
            left: Box::new(Expr::Integer(1)),
            right: Box::new(Expr::Integer(2)),
        };
        let expr2 = expr1.clone();

        assert_eq!(expr1, expr2);
    }

    #[test]
    fn test_complex_expr() {
        let expr = Expr::BinaryOp {
            op: BinaryOperator::Multiply,
            left: Box::new(Expr::BinaryOp {
                op: BinaryOperator::Add,
                left: Box::new(Expr::Integer(2)),
                right: Box::new(Expr::Integer(3)),
            }),
            right: Box::new(Expr::Integer(4)),
        };

        // Test that we can build complex expressions
        match expr {
            Expr::BinaryOp { op, .. } => {
                assert_eq!(op, BinaryOperator::Multiply);
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_array_type_expr() {
        let expr = Expr::ArrayType {
            size: Box::new(Expr::Integer(5)),
            element_type: Box::new(Expr::Identifier("BYTE".to_string())),
        };

        match expr {
            Expr::ArrayType { size, element_type } => {
                assert_eq!(*size, Expr::Integer(5));
                assert_eq!(*element_type, Expr::Identifier("BYTE".to_string()));
            }
            _ => panic!("Expected ArrayType"),
        }
    }

    #[test]
    fn test_tuple_literal() {
        let tuple = Expr::TupleLiteral {
            fields: vec![
                ("x".to_string(), Expr::Integer(1)),
                ("y".to_string(), Expr::Integer(2)),
            ],
        };

        match tuple {
            Expr::TupleLiteral { fields } => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "x");
                assert_eq!(fields[0].1, Expr::Integer(1));
            }
            _ => panic!("Expected TupleLiteral"),
        }
    }

    #[test]
    fn test_function_definition_statement() {
        let stmt = Statement::FunctionDefinition {
            name: "add".to_string(),
            params: vec![
                Parameter {
                    name: "a".to_string(),
                    type_expr: Some(Expr::Identifier("INT".to_string())),
                },
                Parameter {
                    name: "b".to_string(),
                    type_expr: Some(Expr::Identifier("INT".to_string())),
                },
            ],
            returns: vec![Parameter {
                name: "result".to_string(),
                type_expr: Some(Expr::Identifier("INT".to_string())),
            }],
            body: vec![Statement::Assignment {
                name: "result".to_string(),
                value: Expr::BinaryOp {
                    op: BinaryOperator::Add,
                    left: Box::new(Expr::Identifier("a".to_string())),
                    right: Box::new(Expr::Identifier("b".to_string())),
                },
            }],
        };

        match stmt {
            Statement::FunctionDefinition { name, params, .. } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
            }
            _ => panic!("Expected FunctionDefinition"),
        }
    }

    #[test]
    fn test_for_statement() {
        let stmt = Statement::For {
            variable: "i".to_string(),
            iterable: Expr::RangeSet {
                start: Box::new(Expr::Integer(0)),
                step: None,
                end: Box::new(Expr::Integer(10)),
            },
            body: vec![Statement::Empty],
        };

        match stmt {
            Statement::For {
                variable, iterable, ..
            } => {
                assert_eq!(variable, "i");
                match iterable {
                    Expr::RangeSet { .. } => {}
                    _ => panic!("Expected RangeSet"),
                }
            }
            _ => panic!("Expected For"),
        }
    }
}
