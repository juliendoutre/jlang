// Demonstrating type checking

add = (x: INTEGER, y: INTEGER) -> (result: INTEGER) {
    result = x + y
}

// This will cause a type error - wrong number of arguments
z = add(1, 2, 3)

