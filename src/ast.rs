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

    /// Function call: card(A), out(b), in()
    FunctionCall { name: String, args: Vec<Expr> },

    /// Array type: [n]BYTE or [5]BYTE
    ArrayType {
        size: Box<Expr>,
        element_type: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,         // +
    Subtract,    // -
    Equals,      // ==
    Modulo,      // %
    LessThan,    // <
    GreaterThan, // >
    And,         // &
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Type/set definition: A = { 0, 1, 2 }
    Definition { name: String, value: Expr },

    /// Variable assignment: a = card(A)
    Assignment { name: String, value: Expr },

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
