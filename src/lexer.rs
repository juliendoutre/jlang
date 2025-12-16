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

        // Check for comment
        if let Some(&ch) = self.peek()
            && ch == '/'
        {
            self.advance();
            if let Some(&next_ch) = self.peek() {
                if next_ch == '/' {
                    self.advance();
                    self.skip_comment();
                    return self.next_token(); // Recursively get the next token
                } else {
                    // Just a single slash - not supported yet, treat as error
                    // For now, skip and continue
                    return self.next_token();
                }
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
            Some('%') => Token::new(TokenType::Percent, position),
            Some('<') => Token::new(TokenType::LessThan, position),
            Some('>') => Token::new(TokenType::GreaterThan, position),
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
                    // Single dot - not in our spec, skip it
                    self.next_token()
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
                Token::new(TokenType::Identifier(identifier), position)
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
