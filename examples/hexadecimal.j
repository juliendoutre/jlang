// Defining sets used in this program.
BYTE = { 0, ..., 255 }
HEXADECIMAL_ALPHABET = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' }

// Reading a single byte from the standard input.
n: BYTE = in()

// Reading `n` bytes from the standard input.
input: [n]BYTE = in()

// Defining a function to encode a given string to its hexadecimal representation.
encode = (input: [n: BYTE]BYTE) -> (encoded: [2*n]BYTE) {
    // TODO: later
}

// Calling a function and printing out the result.
out(encode(input))
