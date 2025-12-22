// Demonstration of importing and using external modules

import "file://./examples/math_module.j" sha256 "5050688d2368930b5216537d38f00da08e34becd59a2b66ac5d16a7281b7c589" as math

INTEGER = {-2 ^ 31, ..., 2 ^ 31 - 1}

// Use the imported add function
x: INTEGER = 10
y: INTEGER = 20
sum: INTEGER = math::add(x, y)
stdout(sum)

// Use the imported multiply function
product: INTEGER = math::multiply(x, y)
stdout(product)

// Use the imported factorial function
n: INTEGER = 5
fact: INTEGER = math::factorial(n)
stdout(fact)

// Use the imported power function
base: INTEGER = 2
exponent: INTEGER = 8
result: INTEGER = math::power(base, exponent)
stdout(result)
