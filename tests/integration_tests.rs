use jlang::interpreter::Interpreter;
use jlang::lexer::Lexer;
use jlang::parser::Parser;

fn run_program(source: &str) -> Result<(), String> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new_with_source(lexer, source.to_string());
    let program = parser.parse().map_err(|e| e.message)?;
    let mut interpreter = Interpreter::new();
    interpreter.execute(&program).map_err(|e| e.message)
}

#[test]
fn test_fibonacci_program() {
    let source = r#"
// Defining a set for valid input numbers.
N = { 0, ..., 10 }

// Defining a recursive function to compute the n-th Fibonacci number.
fib = (x: N) -> (result: N) {
    // Base cases
    if x == 0 {
        result = 0
    }

    if x == 1 {
        result = 1
    }

    // Recursive case: fib(n) = fib(n-1) + fib(n-2)
    if x >= 2 {
        result = fib(x - 1) + fib(x - 2)
    }
}

// Test the function
n = 5
result = fib(n)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_dot_product_program() {
    let source = r#"
INTEGER = {-100, ..., 100}

V = { (x: INTEGER, y: INTEGER, z: INTEGER) }

dot_product = (v_1: V, v_2: V) -> (out: INTEGER) {
    out = v_1.x * v_2.x + v_1.y * v_2.y + v_1.z * v_2.z
}

A = (x: 3, y: -8, z: 0)
B = (x: -4, y: 2, z: A.z)
result = dot_product(A, B)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_simple_arithmetic() {
    let source = r#"
a = 10
b = 20
c = a + b
d = c * 2
e = d / 4
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_array_operations() {
    let source = r#"
// Create array
arr = [1, 2, 3, 4, 5]

// Index access
first = arr[0]
last = arr[4]

// Modify array
arr[2] = 10

// Slice
sub = arr[1:4]
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_set_operations() {
    let source = r#"
// Explicit set
S1 = {1, 2, 3, 4, 5}

// Range set
S2 = {0, ..., 10}

// Range set with step
S3 = {0, 2, ..., 10}

// Set cardinality
n1 = card(S1)
n2 = card(S2)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_for_loops() {
    let source = r#"
sum = 0
for i in [1, 2, 3, 4, 5] {
    sum = sum + i
}

product = 1
for i in {1, ..., 5} {
    product = product * i
}
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_nested_loops() {
    let source = r#"
total = 0
for i in [1, 2, 3] {
    for j in [1, 2, 3] {
        total = total + i * j
    }
}
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_if_statements() {
    let source = r#"
x = 10
result = 0

if x > 5 {
    result = 1
}

if x < 5 {
    result = 2
}

if x == 10 {
    result = 3
}
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_function_with_multiple_params() {
    let source = r#"
add = (a: INTEGER, b: INTEGER) -> (result: INTEGER) {
    result = a + b
}

multiply = (a: INTEGER, b: INTEGER) -> (result: INTEGER) {
    result = a * b
}

x = add(5, 3)
y = multiply(4, 7)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_tuple_operations() {
    let source = r#"
point = (x: 10, y: 20, z: 30)

x_coord = point.x
y_coord = point.y
z_coord = point.z

sum = x_coord + y_coord + z_coord
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_character_literals() {
    let source = r#"
a = 'a'
b = 'Z'
c = '0'

// Characters are converted to integers
sum = a + b + c
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_comparison_operators() {
    let source = r#"
a = 5
b = 10

lt = a < b
gt = a > b
eq = a == b
le = a <= b
ge = a >= b
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_complex_expressions() {
    let source = r#"
// Test operator precedence
result1 = 2 + 3 * 4
result2 = (2 + 3) * 4
result3 = 2 ^ 3 + 1
result4 = (2 + 3) ^ 2

// Test nested expressions
result5 = ((1 + 2) * (3 + 4)) / 7
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_power_operations() {
    let source = r#"
p1 = 2 ^ 0
p2 = 2 ^ 1
p3 = 2 ^ 3
p4 = 2 ^ 10
p5 = 10 ^ 2
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_modulo_operations() {
    let source = r#"
m1 = 10 % 3
m2 = 17 % 5
m3 = 100 % 7
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_array_ranges() {
    let source = r#"
// Simple range
r1 = [0, ..., 5]

// Range with step
r2 = [0, 2, ..., 10]

// Descending range
r3 = [10, ..., 0]
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_implicit_set_with_constraint() {
    let source = r#"
// Even numbers from 0 to 20
EVEN = {x: {0, ..., 20} | x % 2 == 0}

n = card(EVEN)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_function_calling_function() {
    let source = r#"
double = (x: INTEGER) -> (result: INTEGER) {
    result = x * 2
}

quadruple = (x: INTEGER) -> (result: INTEGER) {
    result = double(double(x))
}

value = quadruple(5)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_array_of_expressions() {
    let source = r#"
a = 1
b = 2
c = 3

arr = [a + b, b + c, c + a]
sum = arr[0] + arr[1] + arr[2]
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_empty_collections() {
    let source = r#"
empty_array = []
empty_set = {}

n1 = len(empty_array)
n2 = card(empty_set)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_negative_numbers() {
    let source = r#"
a = -5
b = -10
c = a + b
d = -a
e = --5
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_complex_function() {
    let source = r#"
factorial = (n: INTEGER) -> (result: INTEGER) {
    result = 1
    for i in {1, ..., n} {
        result = result * i
    }
}

fact5 = factorial(5)
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_array_slicing_variations() {
    let source = r#"
arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// Full slice
s1 = arr[0:10]

// Partial slices
s2 = arr[2:5]
s3 = arr[0:3]
s4 = arr[7:10]
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_parse_error_detection() {
    let source = "x = ";
    assert!(run_program(source).is_err());
}

#[test]
fn test_runtime_error_division_by_zero() {
    let source = "x = 10 / 0";
    assert!(run_program(source).is_err());
}

#[test]
fn test_runtime_error_undefined_variable() {
    let source = "x = undefined_var + 1";
    assert!(run_program(source).is_err());
}

#[test]
fn test_runtime_error_array_out_of_bounds() {
    let source = r#"
arr = [1, 2, 3]
x = arr[10]
"#;
    assert!(run_program(source).is_err());
}

#[test]
fn test_multiline_program() {
    let source = r#"
// This is a comment
x = 1

// Another comment
y = 2

// Calculate sum
z = x + y
"#;

    assert!(run_program(source).is_ok());
}

#[test]
fn test_whitespace_handling() {
    let source = "  x   =   42  \n  y  =  x  +  1  ";
    assert!(run_program(source).is_ok());
}

#[test]
fn test_complex_tuple_usage() {
    let source = r#"
point1 = (x: 1, y: 2, z: 3)
point2 = (x: 4, y: 5, z: 6)

// Create new point from fields
point3 = (x: point1.x + point2.x, y: point1.y + point2.y, z: point1.z + point2.z)
"#;

    assert!(run_program(source).is_ok());
}
