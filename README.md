# JLang

## Specification

### Source code

Source code must be an [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoded text.

It's expected to be a sequence of tokens, which can be one of:
- a comment: any sequence of characters following a `//` symbol and ending with a new line (or an EOF).
- an identifier: a sequence of letters, digits or `_` that does not start with a digit.
- a literal: a sequence of digits.
- a symbol: any symbol in the following list:
```
Arithmetic
+ - * / % > >= < <=
Logic
& | ! ^
Assignement
= ->
Punctuation
. , : ...
Grouping
{ } ( ) [ ]
```

Spaces are ignored unless they allow to separate two other tokens (for instance two identifiers), or they are part of a comment.

### Syntax

```jlang
EMPTY = {} // Define an empty set
BOOLEANS = {0, 1} // Define a set by enumerating its values
BYTES = {0, ..., 255} // Define a set with a range
EVEN_BYTES = { 0, 2, ..., 254 } // Define a set with a range and a step
x = 100 // Assign a value to a variable
```
