use crate::ast::{BinaryOperator, Expr, Parameter, Program, Statement, UnaryOperator};
use crate::error;
use crate::module_loader::ModuleLoader;
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Set representation supporting both materialized and lazy evaluation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SetValue {
    /// Materialized set (for small sets or after operations)
    Materialized(HashSet<i64>),
    /// Range-based set (lazy evaluation)
    Range { start: i64, end: i64, step: i64 },
}

impl SetValue {
    /// Get the cardinality (size) of the set without materializing
    fn cardinality(&self) -> Result<usize> {
        match self {
            SetValue::Materialized(set) => Ok(set.len()),
            SetValue::Range { start, end, step } => {
                if *step == 0 {
                    return Err(RuntimeError::new(
                        "Invalid range with zero step".to_string(),
                    ));
                }
                if *step > 0 {
                    if end >= start {
                        Ok(((end - start) / step + 1) as usize)
                    } else {
                        Ok(0)
                    }
                } else if end <= start {
                    Ok(((start - end) / (-step) + 1) as usize)
                } else {
                    Ok(0)
                }
            }
        }
    }

    /// Materialize the set (only when absolutely necessary)
    fn materialize(&self) -> Result<HashSet<i64>> {
        match self {
            SetValue::Materialized(set) => Ok(set.clone()),
            SetValue::Range { start, end, step } => {
                let size = self.cardinality()?;
                if size > 1_000_000 {
                    return Err(RuntimeError::new(format!(
                        "Cannot materialize set with {} elements (too large)",
                        size
                    )));
                }

                let mut set = HashSet::new();
                let mut current = *start;
                if *step > 0 {
                    while current <= *end {
                        set.insert(current);
                        current += step;
                    }
                } else {
                    while current >= *end {
                        set.insert(current);
                        current += step;
                    }
                }
                Ok(set)
            }
        }
    }
}

/// Runtime values in the language
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// An integer value
    Integer(i64),
    /// A set of integers (lazy or materialized)
    Set(SetValue),
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
    /// A tuple with named fields
    Tuple(HashMap<String, i64>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Set(set_val) => match set_val {
                SetValue::Range { start, end, step } => {
                    if *step == 1 {
                        write!(f, "{{{}, ..., {}}}", start, end)
                    } else {
                        write!(f, "{{{}, {}, ..., {}}}", start, start + step, end)
                    }
                }
                SetValue::Materialized(set) => {
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
            },
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
            Value::Tuple(fields) => {
                let mut field_vec: Vec<_> = fields.iter().collect();
                field_vec.sort_by_key(|(k, _)| *k);
                write!(f, "(")?;
                for (i, (name, value)) in field_vec.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, value)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub type RuntimeError = error::RuntimeError;

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
        self.bindings.get(name).cloned().ok_or_else(|| {
            RuntimeError::new(format!("Undefined variable: {}", name))
                .with_suggestion(error::suggest_for_undefined_variable(name))
        })
    }
}

/// Module metadata stored for lazy initialization
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct ModuleInfo {
    url: String,
    checksum: String,
    program: Program,
}

/// The interpreter for executing programs
pub struct Interpreter {
    env: Environment,
    module_loader: ModuleLoader,
    /// Module info by alias (for lazy loading)
    module_info: HashMap<String, ModuleInfo>,
    /// Module environments (initialized lazily)
    module_envs: HashMap<String, Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
            module_loader: ModuleLoader::new(),
            module_info: HashMap::new(),
            module_envs: HashMap::new(),
        }
    }

    /// Execute a program
    pub fn execute(&mut self, program: &Program) -> Result<()> {
        for statement in &program.statements {
            self.execute_statement(statement)?;
        }
        Ok(())
    }

    /// Lazily initialize a module's environment (execute it once, cache result)
    fn ensure_module_initialized(&mut self, alias: &str) -> Result<()> {
        // Check if already initialized
        if self.module_envs.contains_key(alias) {
            return Ok(());
        }

        // Get module info
        let module_info = self
            .module_info
            .get(alias)
            .ok_or_else(|| {
                RuntimeError::new(format!("Module '{}' not imported", alias)).with_suggestion(
                    format!(
                        "Add 'import \"...\" sha256 \"...\" as {}' before using it",
                        alias
                    ),
                )
            })?
            .clone();

        // Create a new interpreter for the module with its own environment
        let mut module_interpreter = Interpreter {
            env: Environment::new(),
            module_loader: ModuleLoader::new(),
            module_info: HashMap::new(),
            module_envs: HashMap::new(),
        };

        // Execute the module to populate its environment
        // ONLY execute definitions - skip side effects like stdout() calls
        for stmt in &module_info.program.statements {
            match stmt {
                // Skip import statements in modules to avoid re-processing
                Statement::Import { .. } => continue,

                // Allow type/set definitions
                Statement::Definition { .. } => {
                    module_interpreter.execute_statement(stmt).map_err(|e| {
                        RuntimeError::new(format!("Error executing module '{}': {}", alias, e))
                    })?;
                }

                // Allow function definitions
                Statement::FunctionDefinition { .. } => {
                    module_interpreter.execute_statement(stmt).map_err(|e| {
                        RuntimeError::new(format!("Error executing module '{}': {}", alias, e))
                    })?;
                }

                // Skip all other statements (side effects)
                // This includes:
                // - ExpressionStatement (stdout, etc)
                // - Assignment without type
                // - TypedAssignment (could have side effects)
                // - Return, For, If, etc.
                _ => {
                    // Optionally warn or error here
                    // For now, silently skip
                }
            }
        }

        // Cache the initialized environment
        self.module_envs
            .insert(alias.to_string(), module_interpreter.env);

        Ok(())
    }

    /// Execute a single statement
    fn execute_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Import {
                url,
                checksum,
                alias,
            } => {
                // Load and parse the module (do NOT execute yet)
                let program = self
                    .module_loader
                    .load_module(url, checksum)
                    .map_err(|e| RuntimeError::new(format!("Failed to import module: {}", e)))?
                    .clone();

                // Store module info for lazy initialization
                self.module_info.insert(
                    alias.clone(),
                    ModuleInfo {
                        url: url.clone(),
                        checksum: checksum.clone(),
                        program,
                    },
                );
            }
            Statement::Definition { name, value } => {
                let val = self.eval_expr(value)?;
                self.env.define(name.clone(), val);
            }
            Statement::Assignment { name, value } => {
                let val = self.eval_expr(value)?;
                self.env.define(name.clone(), val);
            }
            Statement::IndexAssignment {
                array,
                index,
                value,
            } => {
                // Get the array
                let array_val = self.env.get(array)?;

                // Evaluate index and value
                let idx = match self.eval_expr(index)? {
                    Value::Integer(i) => {
                        if i < 0 {
                            return Err(RuntimeError::new(
                                "Array index cannot be negative".to_string(),
                            ));
                        }
                        i as usize
                    }
                    _ => return Err(RuntimeError::new("Index must be an integer".to_string())),
                };

                let new_val = match self.eval_expr(value)? {
                    Value::Integer(i) => i,
                    _ => {
                        return Err(RuntimeError::new(
                            "Array elements must be integers".to_string(),
                        ));
                    }
                };

                // Modify the array
                match array_val {
                    Value::Array(mut arr) => {
                        if idx >= arr.len() {
                            return Err(RuntimeError::new(format!(
                                "Array index {} out of bounds (length: {})",
                                idx,
                                arr.len()
                            ))
                            .with_suggestion(
                                error::suggest_for_array_out_of_bounds(idx, arr.len()),
                            ));
                        }
                        arr[idx] = new_val;
                        self.env.define(array.clone(), Value::Array(arr));
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            "Can only index assign to arrays"
                        ).with_suggestion("Ensure the variable is an array before trying to assign to an index."));
                    }
                }
            }
            Statement::TypedAssignment {
                name,
                type_expr,
                value,
            } => {
                // Check if this is an array type with stdin() call
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
                    && func_name == "stdin"
                    && args.is_empty()
                {
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
            Statement::For {
                variable,
                iterable,
                body,
            } => {
                // Evaluate the iterable
                let iterable_val = self.eval_expr(iterable)?;

                // Save the current value of the loop variable if it exists
                let saved_value = self.env.bindings.get(variable).cloned();

                // Iterate over the values
                match iterable_val {
                    Value::Array(arr) => {
                        for elem in arr {
                            // Bind the loop variable
                            self.env.define(variable.clone(), Value::Integer(elem));

                            // Execute the loop body
                            for stmt in body {
                                self.execute_statement(stmt)?;
                            }
                        }
                    }
                    Value::Set(set_val) => {
                        // Materialize set for iteration
                        let set = set_val.materialize()?;
                        // Convert set to sorted vector for consistent iteration
                        let mut elements: Vec<_> = set.into_iter().collect();
                        elements.sort();

                        for elem in elements {
                            // Bind the loop variable
                            self.env.define(variable.clone(), Value::Integer(elem));

                            // Execute the loop body
                            for stmt in body {
                                self.execute_statement(stmt)?;
                            }
                        }
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            "Can only iterate over arrays and sets".to_string(),
                        ));
                    }
                }

                // Restore the original value of the loop variable
                if let Some(saved) = saved_value {
                    self.env.define(variable.clone(), saved);
                }
            }
            Statement::If { condition, body } => {
                // Evaluate the condition
                let cond_val = self.eval_expr(condition)?;

                // Check if condition is true (non-zero)
                let is_true = match cond_val {
                    Value::Integer(i) => i != 0,
                    _ => {
                        return Err(RuntimeError::new(
                            "Condition must evaluate to an integer".to_string(),
                        ));
                    }
                };

                // Execute body if condition is true
                if is_true {
                    for stmt in body {
                        self.execute_statement(stmt)?;
                    }
                }
            }
            Statement::Empty => {}
        }
        Ok(())
    }

    /// Evaluate an expression
    fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Identifier(name) => self.env.get(name),

            Expr::QualifiedName { module, name } => {
                // Ensure module is initialized (lazy execution)
                self.ensure_module_initialized(module)?;

                // Look up the value in the module's environment
                let module_env = self.module_envs.get(module).unwrap(); // Safe after ensure_module_initialized
                module_env.get(name).map_err(|_| {
                    RuntimeError::new(format!("Name '{}' not found in module '{}'", name, module))
                        .with_suggestion(format!(
                            "Check that '{}' is defined in the imported module",
                            name
                        ))
                })
            }

            Expr::Integer(i) => Ok(Value::Integer(*i)),

            Expr::Character(c) => {
                // Convert character to its UTF-8 encoding value
                Ok(Value::Integer(*c as i64))
            }

            Expr::ExplicitSet(elements) => {
                // Check if this is a type set (contains types) or a value set (contains integers)
                if elements.len() == 1 {
                    // Check if the single element is a tuple type
                    let elem_val = self.eval_expr(&elements[0])?;
                    match elem_val {
                        Value::Type(_) => {
                            // This is a type definition, just return it as a Type
                            return Ok(Value::Type(Box::new(elem_val)));
                        }
                        Value::Integer(i) => {
                            let mut set = HashSet::new();
                            set.insert(i);
                            return Ok(Value::Set(SetValue::Materialized(set)));
                        }
                        _ => {
                            return Err(RuntimeError::new(
                                "Sets can only contain integers or be type definitions".to_string(),
                            ));
                        }
                    }
                }

                // Multiple elements - must be integers
                let mut set = HashSet::new();
                for elem_expr in elements {
                    match self.eval_expr(elem_expr)? {
                        Value::Integer(i) => {
                            set.insert(i);
                        }
                        _ => {
                            return Err(RuntimeError::new(
                                "Multi-element sets can only contain integers".to_string(),
                            ));
                        }
                    }
                }
                Ok(Value::Set(SetValue::Materialized(set)))
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

                // Use lazy range representation instead of materializing
                Ok(Value::Set(SetValue::Range {
                    start: start_val,
                    end: end_val,
                    step: step_val,
                }))
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

                // If there's no constraint, return the type set as-is (lazy!)
                let constraint_expr = match constraint {
                    Some(c) => c,
                    None => return Ok(Value::Set(type_set)),
                };

                // Filter the type set based on the constraint
                // This requires materialization, but we limit the size
                let materialized = type_set.materialize()?;
                let mut result_set = HashSet::new();

                for &elem in &materialized {
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

                Ok(Value::Set(SetValue::Materialized(result_set)))
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
                            return Err(RuntimeError::new("Division by zero")
                                .with_suggestion(error::suggest_for_division_by_zero()));
                        }
                        Ok(Value::Integer(a / b))
                    }
                    (BinaryOperator::Modulo, Value::Integer(a), Value::Integer(b)) => {
                        if b == 0 {
                            return Err(RuntimeError::new("Modulo by zero")
                                .with_suggestion(error::suggest_for_division_by_zero()));
                        }
                        Ok(Value::Integer(a % b))
                    }
                    (BinaryOperator::Power, Value::Integer(a), Value::Integer(b)) => {
                        if b < 0 {
                            return Err(RuntimeError::new(
                                "Negative exponents not supported".to_string(),
                            ));
                        }
                        // Use checked_pow to avoid overflow
                        match a.checked_pow(b as u32) {
                            Some(result) => Ok(Value::Integer(result)),
                            None => Err(RuntimeError::new(
                                "Integer overflow in power operation".to_string(),
                            )),
                        }
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
                    (BinaryOperator::LessThanOrEqual, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(if a <= b { 1 } else { 0 }))
                    }
                    (BinaryOperator::GreaterThanOrEqual, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(if a >= b { 1 } else { 0 }))
                    }
                    (BinaryOperator::And, Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(if a != 0 && b != 0 { 1 } else { 0 }))
                    }

                    // Set operations
                    (BinaryOperator::Subtract, Value::Set(a), Value::Set(b)) => {
                        // Materialize both sets (with size limit)
                        let set_a = a.materialize()?;
                        let set_b = b.materialize()?;
                        let diff: HashSet<_> = set_a.difference(&set_b).cloned().collect();
                        Ok(Value::Set(SetValue::Materialized(diff)))
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
                    Value::Set(set_val) => {
                        // Materialize set for indexing
                        let set = set_val.materialize()?;
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

            Expr::ArrayRange { start, step, end } => {
                let start_val = match self.eval_expr(start)? {
                    Value::Integer(i) => i,
                    _ => {
                        return Err(RuntimeError::new(
                            "Array range start must be an integer".to_string(),
                        ));
                    }
                };

                let end_val = match self.eval_expr(end)? {
                    Value::Integer(i) => i,
                    _ => {
                        return Err(RuntimeError::new(
                            "Array range end must be an integer".to_string(),
                        ));
                    }
                };

                let step_val = if let Some(step_expr) = step {
                    match self.eval_expr(step_expr)? {
                        Value::Integer(i) => i - start_val,
                        _ => {
                            return Err(RuntimeError::new(
                                "Array range step must be an integer".to_string(),
                            ));
                        }
                    }
                } else {
                    // Infer step direction from start and end
                    if start_val <= end_val { 1 } else { -1 }
                };

                if step_val == 0 {
                    return Err(RuntimeError::new(
                        "Array range step cannot be zero".to_string(),
                    ));
                }

                let mut arr = Vec::new();
                let mut current = start_val;

                if step_val > 0 {
                    while current <= end_val {
                        arr.push(current);
                        current += step_val;
                    }
                } else {
                    while current >= end_val {
                        arr.push(current);
                        current += step_val;
                    }
                }

                Ok(Value::Array(arr))
            }

            Expr::Slice { array, start, end } => {
                let array_val = self.eval_expr(array)?;

                match array_val {
                    Value::Array(arr) => {
                        let len = arr.len();

                        let start_idx = if let Some(start_expr) = start {
                            match self.eval_expr(start_expr)? {
                                Value::Integer(i) => {
                                    if i < 0 {
                                        return Err(RuntimeError::new(
                                            "Slice start index cannot be negative".to_string(),
                                        ));
                                    }
                                    i as usize
                                }
                                _ => {
                                    return Err(RuntimeError::new(
                                        "Slice start must be an integer".to_string(),
                                    ));
                                }
                            }
                        } else {
                            0
                        };

                        let end_idx = if let Some(end_expr) = end {
                            match self.eval_expr(end_expr)? {
                                Value::Integer(i) => {
                                    if i < 0 {
                                        return Err(RuntimeError::new(
                                            "Slice end index cannot be negative".to_string(),
                                        ));
                                    }
                                    i as usize
                                }
                                _ => {
                                    return Err(RuntimeError::new(
                                        "Slice end must be an integer".to_string(),
                                    ));
                                }
                            }
                        } else {
                            len
                        };

                        if start_idx > len {
                            return Err(RuntimeError::new(format!(
                                "Slice start index {} out of bounds (array length: {})",
                                start_idx, len
                            )));
                        }

                        if end_idx > len {
                            return Err(RuntimeError::new(format!(
                                "Slice end index {} out of bounds (array length: {})",
                                end_idx, len
                            )));
                        }

                        if start_idx > end_idx {
                            return Err(RuntimeError::new(format!(
                                "Slice start index {} is greater than end index {}",
                                start_idx, end_idx
                            )));
                        }

                        Ok(Value::Array(arr[start_idx..end_idx].to_vec()))
                    }
                    _ => Err(RuntimeError::new("Can only slice arrays".to_string())),
                }
            }

            Expr::TupleType { fields } => {
                // Tuple types are type specifications, not runtime values
                // Store them as a Type value containing a tuple literal representation
                let mut type_fields = HashMap::new();
                for (name, _type_expr) in fields {
                    // For now, just store 0 as a placeholder for the type
                    // In a full type system, we'd store the actual type information
                    type_fields.insert(name.clone(), 0);
                }
                Ok(Value::Type(Box::new(Value::Tuple(type_fields))))
            }

            Expr::TupleLiteral { fields } => {
                let mut tuple_fields = HashMap::new();
                for (name, value_expr) in fields {
                    let value = match self.eval_expr(value_expr)? {
                        Value::Integer(i) => i,
                        _ => {
                            return Err(RuntimeError::new(
                                "Tuple fields must be integers".to_string(),
                            ));
                        }
                    };
                    tuple_fields.insert(name.clone(), value);
                }
                Ok(Value::Tuple(tuple_fields))
            }

            Expr::FieldAccess { object, field } => {
                let obj_val = self.eval_expr(object)?;
                match obj_val {
                    Value::Tuple(fields) => fields
                        .get(field)
                        .copied()
                        .ok_or_else(|| {
                            RuntimeError::new(format!("Tuple has no field named '{}'", field))
                        })
                        .map(Value::Integer),
                    _ => Err(RuntimeError::new(
                        "Field access is only supported on tuples".to_string(),
                    )),
                }
            }

            Expr::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand)?;
                match (op, val) {
                    (UnaryOperator::Negate, Value::Integer(i)) => Ok(Value::Integer(-i)),
                    _ => Err(RuntimeError::new(
                        "Unary operator only supported on integers".to_string(),
                    )),
                }
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
        // Check if it's a qualified function call (module::function)
        if name.contains("::") {
            let parts: Vec<&str> = name.split("::").collect();
            if parts.len() != 2 {
                return Err(RuntimeError::new(format!(
                    "Invalid qualified name: {}",
                    name
                )));
            }

            let module = parts[0];
            let func_name = parts[1];

            // Ensure module is initialized (lazy execution)
            self.ensure_module_initialized(module)?;

            // Get the module's environment and clone what we need
            let (func_value, module_bindings) = {
                let module_env = self.module_envs.get(module).unwrap(); // Safe after ensure_module_initialized

                let func_value = module_env.get(func_name).map_err(|_| {
                    RuntimeError::new(format!(
                        "Function '{}' not found in module '{}'",
                        func_name, module
                    ))
                })?;

                // Clone the module bindings and function value to avoid borrow issues
                (func_value.clone(), module_env.bindings.clone())
            };

            // Call the function
            match func_value {
                Value::Function {
                    params,
                    returns,
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
                            "Function {}::{} expects {} arguments, got {}",
                            module,
                            func_name,
                            params.len(),
                            arg_values.len()
                        )));
                    }

                    // Save current environment and temporarily use module's environment
                    let saved_env = std::mem::take(&mut self.env);

                    // Copy all module bindings into the current environment
                    for (name, value) in module_bindings {
                        self.env.define(name.clone(), value.clone());
                    }

                    // Bind input parameters
                    for (param, value) in params.iter().zip(arg_values.iter()) {
                        self.env.define(param.name.clone(), value.clone());
                    }

                    // Initialize output parameters
                    for return_param in returns.iter() {
                        self.env
                            .define(return_param.name.clone(), Value::Integer(0));
                    }

                    // Execute function body
                    let mut result = Value::Integer(0);
                    for stmt in body.iter() {
                        match stmt {
                            Statement::Return(expr) => {
                                result = self.eval_expr(expr)?;
                                break;
                            }
                            _ => {
                                self.execute_statement(stmt)?;
                            }
                        }
                    }

                    // If no explicit return and we have output parameters, collect them
                    if returns.len() == 1 {
                        result = self.env.get(&returns[0].name)?;
                    }

                    // Restore original environment
                    self.env = saved_env;

                    Ok(result)
                }
                _ => Err(RuntimeError::new(format!(
                    "'{}' in module '{}' is not a function",
                    func_name, module
                ))),
            }
        } else {
            // Regular function call (not qualified)
            self.call_local_function(name, args)
        }
    }

    fn call_local_function(&mut self, name: &str, args: &[Expr]) -> Result<Value> {
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
                    Value::Set(set) => {
                        let size = set.cardinality()?;
                        Ok(Value::Integer(size as i64))
                    }
                    Value::Array(arr) => Ok(Value::Integer(arr.len() as i64)),
                    Value::Integer(_)
                    | Value::Type(_)
                    | Value::Tuple(_)
                    | Value::Function { .. } => Err(RuntimeError::new(
                        "card() expects a set or array argument".to_string(),
                    )),
                }
            }

            "len" => {
                if args.len() != 1 {
                    return Err(RuntimeError::new(format!(
                        "len() expects 1 argument, got {}",
                        args.len()
                    )));
                }
                let arg = self.eval_expr(&args[0])?;
                match arg {
                    Value::Array(arr) => Ok(Value::Integer(arr.len() as i64)),
                    Value::Set(_)
                    | Value::Integer(_)
                    | Value::Type(_)
                    | Value::Tuple(_)
                    | Value::Function { .. } => Err(RuntimeError::new(
                        "len() expects an array argument".to_string(),
                    )),
                }
            }

            "stdout" => {
                if args.len() != 1 {
                    return Err(RuntimeError::new(format!(
                        "stdout() expects 1 argument, got {}",
                        args.len()
                    )));
                }
                let arg = self.eval_expr(&args[0])?;
                println!("{}", arg);
                Ok(arg)
            }

            "stdin" => {
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
                        returns,
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

                        // Save current environment state (both input and output parameters)
                        let saved_bindings: Vec<_> = params
                            .iter()
                            .chain(returns.iter())
                            .filter_map(|p| {
                                self.env
                                    .bindings
                                    .get(&p.name)
                                    .map(|v| (p.name.clone(), v.clone()))
                            })
                            .collect();

                        // Bind input parameters
                        for (param, value) in params.iter().zip(arg_values.iter()) {
                            self.env.define(param.name.clone(), value.clone());
                        }

                        // Initialize output parameters
                        for return_param in returns.iter() {
                            if let Some(type_expr) = &return_param.type_expr {
                                // Check if it's an array type
                                match type_expr {
                                    Expr::ArrayType {
                                        size,
                                        element_type: _,
                                    } => {
                                        // Evaluate the size expression
                                        let size_val = self.eval_expr(size)?;
                                        let array_size = match size_val {
                                            Value::Integer(n) => {
                                                if n < 0 {
                                                    return Err(RuntimeError::new(
                                                        "Array size cannot be negative".to_string(),
                                                    ));
                                                }
                                                n as usize
                                            }
                                            _ => {
                                                return Err(RuntimeError::new(
                                                    "Array size must be an integer".to_string(),
                                                ));
                                            }
                                        };
                                        // Initialize with zeros
                                        let zero_array = vec![0; array_size];
                                        self.env.define(
                                            return_param.name.clone(),
                                            Value::Array(zero_array),
                                        );
                                    }
                                    _ => {
                                        // For non-array types, initialize with 0
                                        self.env
                                            .define(return_param.name.clone(), Value::Integer(0));
                                    }
                                }
                            } else {
                                // No type annotation, initialize with 0
                                self.env
                                    .define(return_param.name.clone(), Value::Integer(0));
                            }
                        }

                        // Execute function body
                        let mut result = Value::Integer(0);
                        let mut explicit_return = false;
                        for (i, stmt) in body.iter().enumerate() {
                            let is_last = i == body.len() - 1;
                            match stmt {
                                Statement::Return(expr) => {
                                    result = self.eval_expr(expr)?;
                                    explicit_return = true;
                                    break;
                                }
                                Statement::ExpressionStatement(expr) if is_last => {
                                    // Last expression statement is implicitly returned
                                    result = self.eval_expr(expr)?;
                                    explicit_return = true;
                                }
                                Statement::Assignment { name, value }
                                | Statement::Definition { name, value } => {
                                    let val = self.eval_expr(value)?;
                                    self.env.define(name.clone(), val.clone());
                                    if is_last {
                                        result = val;
                                        explicit_return = true;
                                    }
                                }
                                _ => {
                                    self.execute_statement(stmt)?;
                                }
                            }
                        }

                        // If no explicit return, return the first output parameter
                        if !explicit_return
                            && !returns.is_empty()
                            && let Some(output_param) = returns.first()
                            && let Ok(val) = self.env.get(&output_param.name)
                        {
                            result = val;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn eval(expr_str: &str) -> Result<Value> {
        let lexer = crate::lexer::Lexer::new(expr_str);
        let mut parser = crate::parser::Parser::new_with_source(lexer, expr_str.to_string());
        let expr = parser
            .parse_expression()
            .map_err(|e| RuntimeError::new(e.message))?;
        let mut interpreter = Interpreter::new();
        interpreter.eval_expr(&expr)
    }

    fn execute(program_str: &str) -> Result<()> {
        let lexer = crate::lexer::Lexer::new(program_str);
        let mut parser = crate::parser::Parser::new_with_source(lexer, program_str.to_string());
        let program = parser.parse().map_err(|e| RuntimeError::new(e.message))?;
        let mut interpreter = Interpreter::new();
        interpreter.execute(&program)
    }

    #[test]
    fn test_eval_integer() {
        let result = eval("42").unwrap();
        assert_eq!(result, Value::Integer(42));
    }

    #[test]
    fn test_eval_character() {
        let result = eval("'a'").unwrap();
        assert_eq!(result, Value::Integer(97)); // ASCII value
    }

    #[test]
    fn test_eval_addition() {
        let result = eval("2 + 3").unwrap();
        assert_eq!(result, Value::Integer(5));
    }

    #[test]
    fn test_eval_subtraction() {
        let result = eval("10 - 3").unwrap();
        assert_eq!(result, Value::Integer(7));
    }

    #[test]
    fn test_eval_multiplication() {
        let result = eval("4 * 5").unwrap();
        assert_eq!(result, Value::Integer(20));
    }

    #[test]
    fn test_eval_division() {
        let result = eval("15 / 3").unwrap();
        assert_eq!(result, Value::Integer(5));
    }

    #[test]
    fn test_eval_division_by_zero() {
        let result = eval("10 / 0");
        assert!(result.is_err());
    }

    #[test]
    fn test_eval_modulo() {
        let result = eval("17 % 5").unwrap();
        assert_eq!(result, Value::Integer(2));
    }

    #[test]
    fn test_eval_power() {
        let result = eval("2 ^ 3").unwrap();
        assert_eq!(result, Value::Integer(8));
    }

    #[test]
    fn test_eval_power_zero() {
        let result = eval("5 ^ 0").unwrap();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_eval_negation() {
        let result = eval("-5").unwrap();
        assert_eq!(result, Value::Integer(-5));
    }

    #[test]
    fn test_eval_comparison_equals() {
        assert_eq!(eval("5 == 5").unwrap(), Value::Integer(1));
        assert_eq!(eval("5 == 6").unwrap(), Value::Integer(0));
    }

    #[test]
    fn test_eval_comparison_less_than() {
        assert_eq!(eval("3 < 5").unwrap(), Value::Integer(1));
        assert_eq!(eval("5 < 3").unwrap(), Value::Integer(0));
    }

    #[test]
    fn test_eval_comparison_greater_than() {
        assert_eq!(eval("5 > 3").unwrap(), Value::Integer(1));
        assert_eq!(eval("3 > 5").unwrap(), Value::Integer(0));
    }

    #[test]
    fn test_eval_comparison_less_or_equal() {
        assert_eq!(eval("3 <= 5").unwrap(), Value::Integer(1));
        assert_eq!(eval("5 <= 5").unwrap(), Value::Integer(1));
        assert_eq!(eval("6 <= 5").unwrap(), Value::Integer(0));
    }

    #[test]
    fn test_eval_and() {
        assert_eq!(eval("1 & 1").unwrap(), Value::Integer(1));
        assert_eq!(eval("1 & 0").unwrap(), Value::Integer(0));
        assert_eq!(eval("0 & 1").unwrap(), Value::Integer(0));
    }

    #[test]
    fn test_eval_array_literal() {
        let result = eval("[1, 2, 3]").unwrap();
        match result {
            Value::Array(arr) => {
                assert_eq!(arr, vec![1, 2, 3]);
            }
            _ => panic!("Expected Array"),
        }
    }

    #[test]
    fn test_eval_array_range() {
        let result = eval("[0, ..., 5]").unwrap();
        match result {
            Value::Array(arr) => {
                assert_eq!(arr, vec![0, 1, 2, 3, 4, 5]);
            }
            _ => panic!("Expected Array"),
        }
    }

    #[test]
    fn test_eval_array_range_with_step() {
        let result = eval("[0, 2, ..., 10]").unwrap();
        match result {
            Value::Array(arr) => {
                assert_eq!(arr, vec![0, 2, 4, 6, 8, 10]);
            }
            _ => panic!("Expected Array"),
        }
    }

    #[test]
    fn test_eval_explicit_set() {
        let result = eval("{1, 2, 3}").unwrap();
        match result {
            Value::Set(SetValue::Materialized(set)) => {
                assert_eq!(set.len(), 3);
                assert!(set.contains(&1));
                assert!(set.contains(&2));
                assert!(set.contains(&3));
            }
            _ => panic!("Expected Set"),
        }
    }

    #[test]
    fn test_eval_range_set() {
        let result = eval("{0, ..., 10}").unwrap();
        match result {
            Value::Set(SetValue::Range { start, end, step }) => {
                assert_eq!(start, 0);
                assert_eq!(end, 10);
                assert_eq!(step, 1);
            }
            _ => panic!("Expected Range Set"),
        }
    }

    #[test]
    fn test_eval_tuple_literal() {
        let result = eval("(x: 3, y: 4)").unwrap();
        match result {
            Value::Tuple(fields) => {
                assert_eq!(fields.get("x"), Some(&3));
                assert_eq!(fields.get("y"), Some(&4));
            }
            _ => panic!("Expected Tuple"),
        }
    }

    #[test]
    fn test_execute_assignment() {
        execute("x = 42").unwrap();
    }

    #[test]
    fn test_execute_definition() {
        execute("X = {1, 2, 3}").unwrap();
    }

    #[test]
    fn test_execute_variable_reference() {
        let program = "x = 5\ny = x + 3";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_if_true() {
        let program = "x = 0\nif 1 { x = 5 }";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_if_false() {
        let program = "x = 0\nif 0 { x = 5 }";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_for_loop_array() {
        let program = "sum = 0\nfor i in [1, 2, 3] { sum = sum + i }";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_for_loop_set() {
        let program = "sum = 0\nfor i in {1, 2, 3} { sum = sum + i }";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_array_indexing() {
        let program = "arr = [10, 20, 30]\nx = arr[1]";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_array_index_assignment() {
        let program = "arr = [1, 2, 3]\narr[1] = 10";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_array_slice() {
        let program = "arr = [1, 2, 3, 4, 5]\nsliced = arr[1:4]";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_field_access() {
        let program = "point = (x: 10, y: 20)\na = point.x";
        execute(program).unwrap();
    }

    #[test]
    fn test_execute_function_definition_and_call() {
        let program =
            "add = (x: INTEGER, y: INTEGER) -> (result: INTEGER) { result = x + y }\nz = add(3, 4)";
        execute(program).unwrap();
    }

    #[test]
    fn test_builtin_card_set() {
        let program = "S = {1, 2, 3, 4, 5}\nn = card(S)";
        execute(program).unwrap();
    }

    #[test]
    fn test_builtin_card_array() {
        let program = "arr = [1, 2, 3]\nn = card(arr)";
        execute(program).unwrap();
    }

    #[test]
    fn test_builtin_len() {
        let program = "arr = [1, 2, 3]\nn = len(arr)";
        execute(program).unwrap();
    }

    #[test]
    fn test_set_cardinality() {
        let set = SetValue::Range {
            start: 0,
            end: 10,
            step: 1,
        };
        assert_eq!(set.cardinality().unwrap(), 11);
    }

    #[test]
    fn test_set_cardinality_negative_step() {
        let set = SetValue::Range {
            start: 10,
            end: 0,
            step: -1,
        };
        assert_eq!(set.cardinality().unwrap(), 11);
    }

    #[test]
    fn test_set_materialize_small() {
        let set = SetValue::Range {
            start: 0,
            end: 5,
            step: 1,
        };
        let materialized = set.materialize().unwrap();
        assert_eq!(materialized.len(), 6);
        assert!(materialized.contains(&0));
        assert!(materialized.contains(&5));
    }

    #[test]
    fn test_environment_define_and_get() {
        let mut env = Environment::new();
        env.define("x".to_string(), Value::Integer(42));
        assert_eq!(env.get("x").unwrap(), Value::Integer(42));
    }

    #[test]
    fn test_environment_undefined_variable() {
        let env = Environment::new();
        assert!(env.get("undefined").is_err());
    }

    #[test]
    fn test_value_display_integer() {
        let val = Value::Integer(42);
        assert_eq!(format!("{}", val), "42");
    }

    #[test]
    fn test_value_display_array() {
        let val = Value::Array(vec![1, 2, 3]);
        assert_eq!(format!("{}", val), "[1, 2, 3]");
    }

    #[test]
    fn test_value_display_tuple() {
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), 1);
        fields.insert("y".to_string(), 2);
        let val = Value::Tuple(fields);
        let display = format!("{}", val);
        assert!(display.contains("x: 1"));
        assert!(display.contains("y: 2"));
    }

    #[test]
    fn test_complex_expression() {
        let result = eval("2 * (3 + 4)").unwrap();
        assert_eq!(result, Value::Integer(14));
    }

    #[test]
    fn test_nested_array_access() {
        let program = "arr = [1, 2, 3]\nx = arr[arr[0]]";
        execute(program).unwrap();
    }

    #[test]
    fn test_fibonacci_like_recursion() {
        let program = r#"
            fib = (n: INTEGER) -> (result: INTEGER) {
                if n == 0 { result = 0 }
                if n == 1 { result = 1 }
                if n >= 2 { result = fib(n - 1) + fib(n - 2) }
            }
            x = fib(5)
        "#;
        execute(program).unwrap();
    }

    #[test]
    fn test_implicit_set_with_constraint() {
        let program = "EVEN = {x: {0, ..., 10} | x % 2 == 0}";
        execute(program).unwrap();
    }

    #[test]
    fn test_array_out_of_bounds() {
        let program = "arr = [1, 2, 3]\nx = arr[10]";
        assert!(execute(program).is_err());
    }

    #[test]
    fn test_negative_power() {
        let result = eval("2 ^ -1");
        assert!(result.is_err());
    }

    #[test]
    fn test_integer_overflow_power() {
        let result = eval("999999 ^ 999999");
        assert!(result.is_err());
    }
}
