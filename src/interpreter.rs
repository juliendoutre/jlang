use crate::ast::{BinaryOperator, Expr, Parameter, Program, Statement};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Runtime values in the language
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// An integer value
    Integer(i64),
    /// A set of integers
    Set(HashSet<i64>),
    /// An array of integers
    Array(Vec<i64>),
    /// A type (used for type constraints)
    Type(Box<Value>),
    /// A function (stored with its definition)
    Function {
        params: Vec<Parameter>,
        returns: Vec<Parameter>,
        body: Vec<Statement>,
    },
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Set(set) => {
                let mut elements: Vec<_> = set.iter().collect();
                elements.sort();
                write!(f, "{{")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "}}")
            }
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, elem) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Value::Type(inner) => write!(f, "Type({})", inner),
            Value::Function { .. } => write!(f, "<function>"),
        }
    }
}

/// Runtime error
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
}

impl RuntimeError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

type Result<T> = std::result::Result<T, RuntimeError>;

/// The interpreter environment storing variable bindings
pub struct Environment {
    bindings: HashMap<String, Value>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<Value> {
        self.bindings
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::new(format!("Undefined variable: {}", name)))
    }
}

/// The interpreter for executing programs
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    /// Execute a program
    pub fn execute(&mut self, program: &Program) -> Result<()> {
        for statement in &program.statements {
            self.execute_statement(statement)?;
        }
        Ok(())
    }

    /// Execute a single statement
    fn execute_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Definition { name, value } => {
                let val = self.eval_expr(value)?;
                self.env.define(name.clone(), val);
            }
            Statement::Assignment { name, value } => {
                let val = self.eval_expr(value)?;
                self.env.define(name.clone(), val);
            }
            Statement::TypedAssignment {
                name,
                type_expr,
                value,
            } => {
                // Check if this is an array type with in() call
                if let (
                    Expr::ArrayType {
                        size,
                        element_type: _,
                    },
                    Expr::FunctionCall {
                        name: func_name,
                        args,
                    },
                ) = (type_expr, value)
                {
                    if func_name == "in" && args.is_empty() {
                        // Special handling for reading arrays
                        let size_val = self.eval_expr(size)?;
                        let array_size = match size_val {
                            Value::Integer(n) => n as usize,
                            _ => {
                                return Err(RuntimeError::new(
                                    "Array size must be an integer".to_string(),
                                ));
                            }
                        };

                        // Read array_size values from stdin
                        let arr = self.read_array_from_stdin(array_size)?;
                        self.env.define(name.clone(), Value::Array(arr));
                        return Ok(());
                    }
                }

                // Evaluate the type expression (for validation or size info)
                let _type_val = self.eval_expr(type_expr)?;

                // Evaluate the value
                let val = self.eval_expr(value)?;

                // TODO: Add type checking here

                self.env.define(name.clone(), val);
            }
            Statement::FunctionDefinition {
                name,
                params,
                returns,
                body,
            } => {
                // Store the function definition
                let func = Value::Function {
                    params: params.clone(),
                    returns: returns.clone(),
                    body: body.clone(),
                };
                self.env.define(name.clone(), func);
            }
            Statement::ExpressionStatement(expr) => {
                self.eval_expr(expr)?;
            }
            Statement::Return(expr) => {
                // For now, just evaluate the expression
                // The actual return is handled in call_function
                self.eval_expr(expr)?;
            }
            Statement::Empty => {}
        }
        Ok(())
    }

    /// Evaluate an expression
    fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Identifier(name) => self.env.get(name),

            Expr::Integer(i) => Ok(Value::Integer(*i)),

            Expr::Character(c) => {
                // Convert character to its UTF-8 encoding value
                Ok(Value::Integer(*c as i64))
            }

            Expr::ExplicitSet(elements) => {
                let mut set = HashSet::new();
                for elem_expr in elements {
                    match self.eval_expr(elem_expr)? {
                        Value::Integer(i) => {
                            set.insert(i);
                        }
                        Value::Set(_)
                        | Value::Array(_)
                        | Value::Type(_)
                        | Value::Function { .. } => {
                            return Err(RuntimeError::new(
                                "Cannot have sets, arrays, types, or functions as elements of a set"
                                    .to_string(),
                            ));
                        }
                    }
                }
                Ok(Value::Set(set))
            }

            Expr::RangeSet { start, step, end } => {
                let start_val = match self.eval_expr(start)? {
                    Value::Integer(i) => i,
                    _ => {
                        return Err(RuntimeError::new(
                            "Range start must be an integer".to_string(),
                        ));
                    }
                };

                let step_val = if let Some(step_expr) = step {
                    match self.eval_expr(step_expr)? {
                        Value::Integer(i) => i - start_val,
                        _ => {
                            return Err(RuntimeError::new(
                                "Range step must be an integer".to_string(),
                            ));
                        }
                    }
                } else {
                    1
                };

                let end_val = match self.eval_expr(end)? {
                    Value::Integer(i) => i,
                    _ => {
                        return Err(RuntimeError::new(
                            "Range end must be an integer".to_string(),
                        ));
                    }
                };

                if step_val == 0 {
                    return Err(RuntimeError::new("Range step cannot be zero".to_string()));
                }

                let mut set = HashSet::new();
                let mut current = start_val;

                if step_val > 0 {
                    while current <= end_val {
                        set.insert(current);
                        current += step_val;
                    }
                } else {
                    while current >= end_val {
                        set.insert(current);
                        current += step_val;
                    }
                }

                Ok(Value::Set(set))
            }

            Expr::ImplicitSet {
                variable,
                type_expr,
                constraint,
            } => {
                // Evaluate the type expression to get a set
                let type_set = match self.eval_expr(type_expr)? {
                    Value::Set(s) => s,
                    _ => {
                        return Err(RuntimeError::new(
                            "Type expression must evaluate to a set".to_string(),
                        ));
                    }
                };

                // If there's no constraint, return the type set as-is
                let constraint_expr = match constraint {
                    Some(c) => c,
                    None => return Ok(Value::Set(type_set)),
                };

                // Filter the type set based on the constraint
                let mut result_set = HashSet::new();
                for &elem in &type_set {
                    // Temporarily bind the variable
                    self.env.define(variable.clone(), Value::Integer(elem));

                    // Evaluate the constraint
                    let constraint_result = self.eval_expr(constraint_expr)?;
                    match constraint_result {
                        Value::Integer(i) => {
                            if i != 0 {
                                result_set.insert(elem);
                            }
                        }
                        _ => {
                            return Err(RuntimeError::new(
                                "Constraint must evaluate to an integer (boolean)".to_string(),
                            ));
                        }
                    }
                }

                Ok(Value::Set(result_set))
            }

            Expr::BinaryOp { op, left, right } => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;

                match (op, left_val, right_val) {
                    // Integer arithmetic
                    (BinaryOperator::Add, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(a + b))
                    }
                    (BinaryOperator::Subtract, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(a - b))
                    }
                    (BinaryOperator::Multiply, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(a * b))
                    }
                    (BinaryOperator::Divide, Value::Integer(a), Value::Integer(b)) => {
                        if b == 0 {
                            return Err(RuntimeError::new("Division by zero".to_string()));
                        }
                        Ok(Value::Integer(a / b))
                    }
                    (BinaryOperator::Modulo, Value::Integer(a), Value::Integer(b)) => {
                        if b == 0 {
                            return Err(RuntimeError::new("Division by zero".to_string()));
                        }
                        Ok(Value::Integer(a % b))
                    }

                    // Integer comparisons (return 0 for false, 1 for true)
                    (BinaryOperator::Equals, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(if a == b { 1 } else { 0 }))
                    }
                    (BinaryOperator::LessThan, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(if a < b { 1 } else { 0 }))
                    }
                    (BinaryOperator::GreaterThan, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(if a > b { 1 } else { 0 }))
                    }
                    (BinaryOperator::And, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(if a != 0 && b != 0 { 1 } else { 0 }))
                    }

                    // Set operations
                    (BinaryOperator::Subtract, Value::Set(a), Value::Set(b)) => {
                        let diff: HashSet<_> = a.difference(&b).cloned().collect();
                        Ok(Value::Set(diff))
                    }

                    _ => Err(RuntimeError::new(format!(
                        "Invalid operands for binary operation: {:?}",
                        op
                    ))),
                }
            }

            Expr::FunctionCall { name, args } => self.call_function(name, args),

            Expr::ArrayType { size, element_type } => {
                // For now, just return the size as an integer
                // This is used in type constraints
                let size_val = self.eval_expr(size)?;
                let _elem_type = self.eval_expr(element_type)?;

                // Return a type representation
                Ok(Value::Type(Box::new(size_val)))
            }

            Expr::TypeConstrained { name, type_expr } => {
                // Evaluate the type expression (for validation)
                let _type_val = self.eval_expr(type_expr)?;

                // Return the identifier's value
                // In array sizes, this acts like a constrained identifier
                Ok(Expr::Identifier(name.clone())).and_then(|expr| self.eval_expr(&expr))
            }

            Expr::Index { array, index } => {
                let array_val = self.eval_expr(array)?;
                let index_val = self.eval_expr(index)?;

                let idx = match index_val {
                    Value::Integer(i) => i as usize,
                    _ => return Err(RuntimeError::new("Index must be an integer".to_string())),
                };

                match array_val {
                    Value::Array(arr) => arr
                        .get(idx)
                        .copied()
                        .map(Value::Integer)
                        .ok_or_else(|| RuntimeError::new("Index out of bounds".to_string())),
                    Value::Set(set) => {
                        // For sets, treat indexing as element selection
                        let mut elements: Vec<_> = set.iter().copied().collect();
                        elements.sort();
                        elements
                            .get(idx)
                            .copied()
                            .map(Value::Integer)
                            .ok_or_else(|| RuntimeError::new("Index out of bounds".to_string()))
                    }
                    _ => Err(RuntimeError::new(
                        "Can only index arrays and sets".to_string(),
                    )),
                }
            }

            Expr::ArrayLiteral(elements) => {
                let mut arr = Vec::new();
                for elem_expr in elements {
                    match self.eval_expr(elem_expr)? {
                        Value::Integer(i) => arr.push(i),
                        _ => {
                            return Err(RuntimeError::new(
                                "Array elements must be integers".to_string(),
                            ));
                        }
                    }
                }
                Ok(Value::Array(arr))
            }
        }
    }

    /// Read an array of values from stdin
    fn read_array_from_stdin(&mut self, size: usize) -> Result<Vec<i64>> {
        use std::io::{self, Read};

        let stdin = io::stdin();
        let mut buffer = vec![0u8; size];

        // Read exactly 'size' bytes
        match stdin.lock().read_exact(&mut buffer) {
            Ok(()) => {
                // Convert bytes to i64 values
                Ok(buffer.into_iter().map(|b| b as i64).collect())
            }
            Err(_) => {
                // If we can't read enough bytes, return zeros
                Ok(vec![0; size])
            }
        }
    }

    /// Call a function (builtin or user-defined)
    fn call_function(&mut self, name: &str, args: &[Expr]) -> Result<Value> {
        // Check for builtin functions first
        match name {
            "card" => {
                if args.len() != 1 {
                    return Err(RuntimeError::new(format!(
                        "card() expects 1 argument, got {}",
                        args.len()
                    )));
                }
                let arg = self.eval_expr(&args[0])?;
                match arg {
                    Value::Set(set) => Ok(Value::Integer(set.len() as i64)),
                    Value::Array(arr) => Ok(Value::Integer(arr.len() as i64)),
                    Value::Integer(_) | Value::Type(_) | Value::Function { .. } => Err(
                        RuntimeError::new("card() expects a set or array argument".to_string()),
                    ),
                }
            }

            "out" => {
                if args.len() != 1 {
                    return Err(RuntimeError::new(format!(
                        "out() expects 1 argument, got {}",
                        args.len()
                    )));
                }
                let arg = self.eval_expr(&args[0])?;
                println!("{}", arg);
                Ok(arg)
            }

            "in" => {
                // Read from standard input
                if args.is_empty() {
                    // Read a single byte/integer
                    use std::io::{self, BufRead};
                    let stdin = io::stdin();
                    let mut line = String::new();

                    if stdin.lock().read_line(&mut line).is_ok() {
                        let trimmed = line.trim();
                        if let Ok(num) = trimmed.parse::<i64>() {
                            return Ok(Value::Integer(num));
                        }
                    }

                    // Return 0 if reading fails or no input
                    Ok(Value::Integer(0))
                } else {
                    // Read an array (not fully implemented)
                    // For now, return an empty array
                    Ok(Value::Array(Vec::new()))
                }
            }

            _ => {
                // Try to call a user-defined function
                let func_val = self.env.get(name)?;
                match func_val {
                    Value::Function {
                        params,
                        returns: _,
                        body,
                    } => {
                        // Evaluate arguments
                        let mut arg_values = Vec::new();
                        for arg_expr in args {
                            arg_values.push(self.eval_expr(arg_expr)?);
                        }

                        // Check argument count
                        if arg_values.len() != params.len() {
                            return Err(RuntimeError::new(format!(
                                "Function {} expects {} arguments, got {}",
                                name,
                                params.len(),
                                arg_values.len()
                            )));
                        }

                        // Save current environment state
                        let saved_bindings: Vec<_> = params
                            .iter()
                            .filter_map(|p| {
                                self.env
                                    .bindings
                                    .get(&p.name)
                                    .map(|v| (p.name.clone(), v.clone()))
                            })
                            .collect();

                        // Bind parameters
                        for (param, value) in params.iter().zip(arg_values.iter()) {
                            self.env.define(param.name.clone(), value.clone());
                        }

                        // Execute function body
                        let mut result = Value::Integer(0);
                        for (i, stmt) in body.iter().enumerate() {
                            let is_last = i == body.len() - 1;
                            match stmt {
                                Statement::Return(expr) => {
                                    result = self.eval_expr(expr)?;
                                    break;
                                }
                                Statement::ExpressionStatement(expr) if is_last => {
                                    // Last expression statement is implicitly returned
                                    result = self.eval_expr(expr)?;
                                }
                                Statement::Assignment { name, value }
                                | Statement::Definition { name, value } => {
                                    let val = self.eval_expr(value)?;
                                    self.env.define(name.clone(), val.clone());
                                    if is_last {
                                        result = val;
                                    }
                                }
                                _ => {
                                    self.execute_statement(stmt)?;
                                }
                            }
                        }

                        // Restore environment
                        for (name, value) in saved_bindings {
                            self.env.define(name, value);
                        }

                        Ok(result)
                    }
                    _ => Err(RuntimeError::new(format!("{} is not a function", name))),
                }
            }
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
