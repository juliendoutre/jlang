// A simple math module for testing imports
INTEGER = {-2 ^ 31, ..., 2 ^ 31 - 1}

// Add two integers
add = (x: INTEGER, y: INTEGER) -> (result: INTEGER) {
    result = x + y
}

// Multiply two integers
multiply = (x: INTEGER, y: INTEGER) -> (result: INTEGER) {
    result = x * y
}

// Calculate factorial
factorial = (n: INTEGER) -> (result: INTEGER) {
    if n == 0 {
        result = 1
    }

    if n == 1 {
        result = 1
    }

    if n >= 2 {
        result = n * factorial(n - 1)
    }
}

// Calculate power
power = (base: INTEGER, exponent: INTEGER) -> (result: INTEGER) {
    if exponent == 0 {
        result = 1
    }

    if exponent == 1 {
        result = base
    }

    if exponent >= 2 {
        result = base * power(base, exponent - 1)
    }
}
