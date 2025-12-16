// Comments start with two slashes and end with a jump line.

// Defining the empty set.
// By convention, set names are uppercase.
EMPTY = {} 

// Defining a set by expliciting its elements.
SINGLE = { 0 }
SMALL = { 0, 1, 2, 3 }

// Defining a set for with an inclusive range.
// The step is the difference between the two first items.
// The default step for a range is 1.
B = {0, 1, ..., 5 }
BYTE = {0, ..., 255} 

// Alias a type with a different name.
C = BYTE

// Defining a set implicitely.
D = { x: BYTE } 

// Implicit sets can be further defined with constraints.
EVEN = { x: BYTE | x % 2 == 0 } 

// Binary operations can be applied to sets to obtain new sets.
ODD = BYTE - EVEN

// Applying a function to a set and saving the result to a variable.
// `card` is a builtin function returning the cardinality (number of elements) in a set.
a = card(A)

// Setting a variable to a litteral value.
b = 100 

// Applying a function to a variable.
// `out` is a builtin function printing out the value to the standard output.
out(b)
