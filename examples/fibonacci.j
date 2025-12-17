// Defining a set for valid input numbers.
N = { 0, ..., 30 }

// Reading a number from the standard input.
n: N = stdin()

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

// Computing and printing the n-th Fibonacci number.
stdout(fib(n))
