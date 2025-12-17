// Example demonstrating for loops, array ranges, and slicing.

// Create an array using range syntax.
numbers = [0, ..., 9]

// Sum all numbers using a for loop.
sum = 0

for n in numbers {
    sum = sum + n
}

stdout(sum)

// Create evens array and iterate over it.
evens = [0, 2, ..., 8]

for e in evens {
    stdout(e)
}

// Nested loops with slicing.
for i in [0, 1, 2] {
    end = i + 3
    
    for j in numbers[i:end] {
        stdout(j)
    }
}
