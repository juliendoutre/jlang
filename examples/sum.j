// Defining a set for valid input numbers.
BYTE = { 0, ..., 255 }

// Reading 10 bytes from the standard input.
input: [10]BYTE = stdin()

// Initializing the sum to 0.
sum = 0

// Summing all elements in the input list.
for i in [0, ..., len(input) - 1] {
    sum = sum + input[i]
}

// Printing out the result.
stdout(sum)
