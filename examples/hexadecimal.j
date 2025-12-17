// Defining sets used in this program.
BYTE = { 0, ..., 255 }
HEXADECIMAL_ALPHABET = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' }

// Reading a single byte from the standard input.
n: BYTE = stdin()

// Reading `n` bytes from the standard input.
input: [n]BYTE = stdin()

// Defining a function to encode a given string to its hexadecimal representation.
encode = (input: [n: BYTE]BYTE) -> (encoded: [2*n]BYTE) {
    // Convert each byte to two hex digits
    for i in [0, ..., n - 1] {
        // Get high and low nibbles
        high = input[i] / 16
        low = input[i] % 16
        
        // Convert high nibble to hex character
        high_char = 0

        if high < 10 {
            high_char = 48 + high
        }

        if high >= 10 {
            high_char = 87 + high
        }
        
        // Convert low nibble to hex character
        low_char = 0

        if low < 10 {
            low_char = 48 + low
        }

        if low >= 10 {
            low_char = 87 + low
        }
        
        // Store in encoded array
        encoded[2*i] = high_char
        encoded[2*i + 1] = low_char
    }
}

// Calling a function and printing out the result.
stdout(encode(input))
