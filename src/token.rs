/// Represents a position in the source file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

/// Token types for the language
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Keywords
    For,
    In,
    If,

    // Literals
    Identifier(String),
    Integer(i64),
    Character(char),

    // Punctuation
    LeftBrace,    // {
    RightBrace,   // }
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Colon,        // :
    Pipe,         // |
    Dot,          // .

    // Operators
    Equals,             // =
    DoubleEquals,       // ==
    Plus,               // +
    Minus,              // -
    Star,               // *
    Slash,              // /
    Caret,              // ^
    Arrow,              // ->
    Ellipsis,           // ...
    Percent,            // %
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=
    Ampersand,          // &
    QuestionMark,       // ?
    Underscore,         // _

    // Special
    Newline,
    Eof,
}

/// A token with its type and position
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
}

impl Token {
    pub fn new(token_type: TokenType, position: Position) -> Self {
        Self {
            token_type,
            position,
        }
    }
}
