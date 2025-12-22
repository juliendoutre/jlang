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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_new() {
        let pos = Position::new(5, 10);
        assert_eq!(pos.line, 5);
        assert_eq!(pos.column, 10);
    }

    #[test]
    fn test_position_equality() {
        let pos1 = Position::new(1, 1);
        let pos2 = Position::new(1, 1);
        let pos3 = Position::new(2, 1);

        assert_eq!(pos1, pos2);
        assert_ne!(pos1, pos3);
    }

    #[test]
    fn test_token_type_equality() {
        assert_eq!(TokenType::For, TokenType::For);
        assert_eq!(TokenType::LeftBrace, TokenType::LeftBrace);
        assert_eq!(TokenType::Integer(42), TokenType::Integer(42));
        assert_ne!(TokenType::Integer(42), TokenType::Integer(43));
        assert_eq!(
            TokenType::Identifier("foo".to_string()),
            TokenType::Identifier("foo".to_string())
        );
        assert_ne!(
            TokenType::Identifier("foo".to_string()),
            TokenType::Identifier("bar".to_string())
        );
    }

    #[test]
    fn test_token_new() {
        let pos = Position::new(1, 1);
        let token = Token::new(TokenType::For, pos);

        assert_eq!(token.token_type, TokenType::For);
        assert_eq!(token.position, pos);
    }

    #[test]
    fn test_token_with_value() {
        let pos = Position::new(2, 5);
        let token = Token::new(TokenType::Integer(123), pos);

        assert_eq!(token.token_type, TokenType::Integer(123));
        assert_eq!(token.position.line, 2);
        assert_eq!(token.position.column, 5);
    }

    #[test]
    fn test_token_equality() {
        let pos1 = Position::new(1, 1);
        let pos2 = Position::new(1, 1);
        let pos3 = Position::new(2, 2);

        let token1 = Token::new(TokenType::For, pos1);
        let token2 = Token::new(TokenType::For, pos2);
        let token3 = Token::new(TokenType::For, pos3);
        let token4 = Token::new(TokenType::In, pos1);

        assert_eq!(token1, token2);
        assert_ne!(token1, token3); // Different positions
        assert_ne!(token1, token4); // Different token types
    }

    #[test]
    fn test_token_clone() {
        let pos = Position::new(3, 7);
        let token1 = Token::new(TokenType::Identifier("test".to_string()), pos);
        let token2 = token1.clone();

        assert_eq!(token1, token2);
    }

    #[test]
    fn test_character_token() {
        let pos = Position::new(1, 1);
        let token = Token::new(TokenType::Character('a'), pos);

        assert_eq!(token.token_type, TokenType::Character('a'));
    }
}
