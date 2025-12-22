use crate::token::{Position, Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

/// Lexer that converts source code into tokens
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            position: Position::new(1, 1),
        }
    }

    /// Advance to the next character
    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.input.next() {
            if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
            Some(ch)
        } else {
            None
        }
    }

    /// Peek at the next character without consuming it
    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    /// Skip whitespace (but not newlines)
    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.peek() {
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Skip a comment
    fn skip_comment(&mut self) {
        // Skip the two slashes
        self.advance();
        self.advance();

        // Skip until newline
        while let Some(&ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    /// Parse an identifier or keyword
    fn parse_identifier(&mut self, first_char: char) -> String {
        let mut result = String::new();
        result.push(first_char);

        while let Some(&ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        result
    }

    /// Parse a number
    fn parse_number(&mut self, first_char: char) -> i64 {
        let mut result = String::new();
        result.push(first_char);

        while let Some(&ch) = self.peek() {
            if ch.is_ascii_digit() {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        result.parse().unwrap_or(0)
    }

    /// Parse a character literal (e.g., 'a')
    fn parse_character(&mut self) -> Option<char> {
        // We've already consumed the opening '

        // Get the character
        let ch = self.advance()?;

        // Check for escape sequences
        let result = if ch == '\\' {
            match self.advance()? {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '\'' => '\'',
                '"' => '"',
                '0' => '\0',
                c => c, // Unknown escape, just use the character
            }
        } else {
            ch
        };

        // Consume the closing '
        if let Some(&'\'') = self.peek() {
            self.advance();
        }

        Some(result)
    }

    /// Get the next token
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let position = self.position;

        // Check for comment or division
        if let Some(&ch) = self.peek()
            && ch == '/'
        {
            self.advance();
            if let Some(&next_ch) = self.peek() {
                if next_ch == '/' {
                    self.skip_comment();
                    return self.next_token(); // Recursively get the next token
                } else {
                    // Single slash - division operator
                    return Token::new(TokenType::Slash, position);
                }
            } else {
                // Slash at end of file
                return Token::new(TokenType::Slash, position);
            }
        }

        match self.advance() {
            None => Token::new(TokenType::Eof, position),
            Some('\n') => Token::new(TokenType::Newline, position),
            Some('{') => Token::new(TokenType::LeftBrace, position),
            Some('}') => Token::new(TokenType::RightBrace, position),
            Some('(') => Token::new(TokenType::LeftParen, position),
            Some(')') => Token::new(TokenType::RightParen, position),
            Some('[') => Token::new(TokenType::LeftBracket, position),
            Some(']') => Token::new(TokenType::RightBracket, position),
            Some(',') => Token::new(TokenType::Comma, position),
            Some(':') => Token::new(TokenType::Colon, position),
            Some('|') => Token::new(TokenType::Pipe, position),
            Some('+') => Token::new(TokenType::Plus, position),
            Some('*') => Token::new(TokenType::Star, position),
            Some('^') => Token::new(TokenType::Caret, position),
            Some('%') => Token::new(TokenType::Percent, position),
            Some('<') => {
                // Check for <=
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Token::new(TokenType::LessThanOrEqual, position)
                } else {
                    Token::new(TokenType::LessThan, position)
                }
            }
            Some('>') => {
                // Check for >=
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Token::new(TokenType::GreaterThanOrEqual, position)
                } else {
                    Token::new(TokenType::GreaterThan, position)
                }
            }
            Some('&') => Token::new(TokenType::Ampersand, position),
            Some('?') => Token::new(TokenType::QuestionMark, position),
            Some('_') => Token::new(TokenType::Underscore, position),
            Some('=') => {
                // Check for ==
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Token::new(TokenType::DoubleEquals, position)
                } else {
                    Token::new(TokenType::Equals, position)
                }
            }
            Some('.') => {
                // Check for ...
                if let Some(&'.') = self.peek() {
                    self.advance();
                    if let Some(&'.') = self.peek() {
                        self.advance();
                        Token::new(TokenType::Ellipsis, position)
                    } else {
                        // Just two dots - not valid, but return ellipsis anyway
                        Token::new(TokenType::Ellipsis, position)
                    }
                } else {
                    // Single dot - for field access
                    Token::new(TokenType::Dot, position)
                }
            }
            Some('-') => {
                // Check for ->
                if let Some(&'>') = self.peek() {
                    self.advance();
                    Token::new(TokenType::Arrow, position)
                } else {
                    Token::new(TokenType::Minus, position)
                }
            }
            Some('\'') => {
                // Character literal
                if let Some(ch) = self.parse_character() {
                    Token::new(TokenType::Character(ch), position)
                } else {
                    // Invalid character literal, skip it
                    self.next_token()
                }
            }
            Some(ch) if ch.is_alphabetic() => {
                let identifier = self.parse_identifier(ch);
                // Check for keywords
                let token_type = match identifier.as_str() {
                    "for" => TokenType::For,
                    "in" => TokenType::In,
                    "if" => TokenType::If,
                    _ => TokenType::Identifier(identifier),
                };
                Token::new(token_type, position)
            }
            Some(ch) if ch.is_ascii_digit() => {
                let number = self.parse_number(ch);
                Token::new(TokenType::Integer(number), position)
            }
            Some(_) => {
                // Unknown character, skip it
                self.next_token()
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token.token_type {
            TokenType::Eof => None,
            _ => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        assert_eq!(lexer.next_token().token_type, TokenType::Eof);
    }

    #[test]
    fn test_single_character_tokens() {
        let input = "{ } ( ) [ ] , : . | + - * / % ^ < > = & ? _";
        let mut lexer = Lexer::new(input);

        let expected = vec![
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBracket,
            TokenType::RightBracket,
            TokenType::Comma,
            TokenType::Colon,
            TokenType::Dot,
            TokenType::Pipe,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Percent,
            TokenType::Caret,
            TokenType::LessThan,
            TokenType::GreaterThan,
            TokenType::Equals,
            TokenType::Ampersand,
            TokenType::QuestionMark,
            TokenType::Underscore,
        ];

        for expected_type in expected {
            assert_eq!(lexer.next_token().token_type, expected_type);
        }
    }

    #[test]
    fn test_multi_character_operators() {
        let input = "== <= >= -> ...";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().token_type, TokenType::DoubleEquals);
        assert_eq!(lexer.next_token().token_type, TokenType::LessThanOrEqual);
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::GreaterThanOrEqual
        );
        assert_eq!(lexer.next_token().token_type, TokenType::Arrow);
        assert_eq!(lexer.next_token().token_type, TokenType::Ellipsis);
    }

    #[test]
    fn test_keywords() {
        let input = "for in if";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().token_type, TokenType::For);
        assert_eq!(lexer.next_token().token_type, TokenType::In);
        assert_eq!(lexer.next_token().token_type, TokenType::If);
    }

    #[test]
    fn test_identifiers() {
        let input = "foo bar test123 CamelCase UPPER_CASE test_var";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("foo".to_string())
        );
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("bar".to_string())
        );
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("test123".to_string())
        );
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("CamelCase".to_string())
        );
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("UPPER_CASE".to_string())
        );
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("test_var".to_string())
        );
    }

    #[test]
    fn test_integers() {
        let input = "0 42 123456789";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().token_type, TokenType::Integer(0));
        assert_eq!(lexer.next_token().token_type, TokenType::Integer(42));
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Integer(123456789)
        );
    }

    #[test]
    fn test_characters() {
        let input = "'a' 'Z' '0' ' '";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().token_type, TokenType::Character('a'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('Z'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('0'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character(' '));
    }

    #[test]
    fn test_character_escape_sequences() {
        let input = "'\\n' '\\t' '\\r' '\\\\' '\\'' '\\\"' '\\0'";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().token_type, TokenType::Character('\n'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('\t'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('\r'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('\\'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('\''));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('"'));
        assert_eq!(lexer.next_token().token_type, TokenType::Character('\0'));
    }

    #[test]
    fn test_comments() {
        let input = "foo // this is a comment\nbar";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("foo".to_string())
        );
        assert_eq!(lexer.next_token().token_type, TokenType::Newline);
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("bar".to_string())
        );
    }

    #[test]
    fn test_division_vs_comment() {
        let input = "a/b // comment\n10/2";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("a".to_string())
        );
        assert_eq!(lexer.next_token().token_type, TokenType::Slash);
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("b".to_string())
        );
        assert_eq!(lexer.next_token().token_type, TokenType::Newline);
        assert_eq!(lexer.next_token().token_type, TokenType::Integer(10));
        assert_eq!(lexer.next_token().token_type, TokenType::Slash);
        assert_eq!(lexer.next_token().token_type, TokenType::Integer(2));
    }

    #[test]
    fn test_newlines() {
        let input = "a\nb\n\nc";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("a".to_string())
        );
        assert_eq!(lexer.next_token().token_type, TokenType::Newline);
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("b".to_string())
        );
        assert_eq!(lexer.next_token().token_type, TokenType::Newline);
        assert_eq!(lexer.next_token().token_type, TokenType::Newline);
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("c".to_string())
        );
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "  a  \t  b  ";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("a".to_string())
        );
        assert_eq!(
            lexer.next_token().token_type,
            TokenType::Identifier("b".to_string())
        );
        assert_eq!(lexer.next_token().token_type, TokenType::Eof);
    }

    #[test]
    fn test_iterator_trait() {
        let input = "a b c";
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();

        assert_eq!(tokens.len(), 3);
        assert_eq!(
            tokens[0].token_type,
            TokenType::Identifier("a".to_string())
        );
        assert_eq!(
            tokens[1].token_type,
            TokenType::Identifier("b".to_string())
        );
        assert_eq!(
            tokens[2].token_type,
            TokenType::Identifier("c".to_string())
        );
    }

    #[test]
    fn test_position_tracking() {
        let input = "a\nbc";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token();
        assert_eq!(token1.position.line, 1);
        assert_eq!(token1.position.column, 1);

        let token2 = lexer.next_token(); // newline
        assert_eq!(token2.position.line, 1);

        let token3 = lexer.next_token();
        assert_eq!(token3.position.line, 2);
        assert_eq!(token3.position.column, 1);
    }

    #[test]
    fn test_complex_expression() {
        let input = "fib = (x: N) -> (result: N) { result = x + 1 }";
        let mut lexer = Lexer::new(input);

        // Just verify we can tokenize it without panicking
        while lexer.next_token().token_type != TokenType::Eof {}
    }
}
