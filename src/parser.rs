use crate::ast::{BinaryOperator, Expr, Program, Statement};
use crate::token::{Token, TokenType};
use std::iter::Peekable;

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    /// Peek at the next token
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    /// Consume and return the next token
    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    /// Skip newlines
    fn skip_newlines(&mut self) {
        while let Some(token) = self.peek() {
            if matches!(token.token_type, TokenType::Newline) {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Expect a specific token type
    fn expect(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        match self.advance() {
            Some(token) if std::mem::discriminant(&token.token_type) == std::mem::discriminant(&expected) => {
                Ok(token)
            }
            Some(token) => Err(ParseError::new(format!(
                "Expected {:?}, found {:?}",
                expected, token.token_type
            ))),
            None => Err(ParseError::new("Unexpected end of input")),
        }
    }

    /// Parse the entire program
    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        self.skip_newlines();

        while self.peek().is_some() {
            let stmt = self.parse_statement()?;
            program.add_statement(stmt);
            self.skip_newlines();
        }

        Ok(program)
    }

    /// Parse a statement
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        self.skip_newlines();

        match self.peek() {
            None => Ok(Statement::Empty),
            Some(token) => match &token.token_type {
                TokenType::Identifier(name) => {
                    let name = name.clone();
                    self.advance();

                    // Check if this is a definition/assignment or a function call
                    match self.peek() {
                        Some(Token {
                            token_type: TokenType::Equals,
                            ..
                        }) => {
                            self.advance(); // consume =
                            let value = self.parse_expression()?;
                            
                            // Determine if it's a definition or assignment based on naming convention
                            // Uppercase = definition, lowercase = assignment
                            if name.chars().next().unwrap().is_uppercase() {
                                Ok(Statement::Definition { name, value })
                            } else {
                                Ok(Statement::Assignment { name, value })
                            }
                        }
                        Some(Token {
                            token_type: TokenType::LeftParen,
                            ..
                        }) => {
                            // Function call
                            self.advance(); // consume (
                            let args = self.parse_function_args()?;
                            self.expect(TokenType::RightParen)?;
                            Ok(Statement::ExpressionStatement(Expr::FunctionCall {
                                name,
                                args,
                            }))
                        }
                        _ => Err(ParseError::new(format!(
                            "Unexpected token after identifier '{}'",
                            name
                        ))),
                    }
                }
                _ => Err(ParseError::new("Expected identifier at start of statement")),
            },
        }
    }

    /// Parse function arguments
    fn parse_function_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        // Check for empty args
        if let Some(Token {
            token_type: TokenType::RightParen,
            ..
        }) = self.peek()
        {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);

            match self.peek() {
                Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) => {
                    self.advance();
                }
                _ => break,
            }
        }

        Ok(args)
    }

    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(0)
    }

    /// Parse binary expressions with precedence
    fn parse_binary_expr(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_primary_expr()?;

        while let Some(token) = self.peek() {
            let (op, precedence) = match &token.token_type {
                TokenType::Minus => (BinaryOperator::Subtract, 1),
                TokenType::Plus => (BinaryOperator::Add, 1),
                TokenType::Percent => (BinaryOperator::Modulo, 2),
                TokenType::DoubleEquals => (BinaryOperator::Equals, 0),
                TokenType::LessThan => (BinaryOperator::LessThan, 0),
                TokenType::GreaterThan => (BinaryOperator::GreaterThan, 0),
                TokenType::Ampersand => (BinaryOperator::And, 0),
                TokenType::Pipe => {
                    // Need to check if this is inside a set constraint
                    break;
                }
                _ => break,
            };

            if precedence < min_precedence {
                break;
            }

            self.advance(); // consume operator

            let right = self.parse_binary_expr(precedence + 1)?;

            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    /// Parse primary expressions
    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.advance() {
            Some(Token {
                token_type: TokenType::Integer(n),
                ..
            }) => Ok(Expr::Integer(n)),
            Some(Token {
                token_type: TokenType::Identifier(name),
                ..
            }) => {
                // Check for function call
                if let Some(Token {
                    token_type: TokenType::LeftParen,
                    ..
                }) = self.peek()
                {
                    self.advance(); // consume (
                    let args = self.parse_function_args()?;
                    self.expect(TokenType::RightParen)?;
                    Ok(Expr::FunctionCall { name, args })
                } else {
                    Ok(Expr::Identifier(name))
                }
            }
            Some(Token {
                token_type: TokenType::LeftBrace,
                ..
            }) => self.parse_set_expr(),
            Some(token) => Err(ParseError::new(format!(
                "Unexpected token in expression: {:?}",
                token.token_type
            ))),
            None => Err(ParseError::new("Unexpected end of input")),
        }
    }

    /// Parse set expressions: { ... }
    fn parse_set_expr(&mut self) -> Result<Expr, ParseError> {
        // Check for empty set
        if let Some(Token {
            token_type: TokenType::RightBrace,
            ..
        }) = self.peek()
        {
            self.advance();
            return Ok(Expr::ExplicitSet(Vec::new()));
        }

        // Try to determine what kind of set this is
        // Look ahead to see if we have: x: Type or just elements

        let first_token = self.peek().cloned();
        
        match first_token {
            Some(Token {
                token_type: TokenType::Identifier(var_name),
                ..
            }) => {
                self.advance();
                
                // Check for : (implicit or constrained set)
                if let Some(Token {
                    token_type: TokenType::Colon,
                    ..
                }) = self.peek()
                {
                    self.advance(); // consume :
                    let type_expr = self.parse_expression()?;
                    
                    // Check for | (constraint)
                    if let Some(Token {
                        token_type: TokenType::Pipe,
                        ..
                    }) = self.peek()
                    {
                        self.advance(); // consume |
                        let constraint = self.parse_expression()?;
                        self.expect(TokenType::RightBrace)?;
                        Ok(Expr::ConstrainedSet {
                            variable: var_name,
                            type_expr: Box::new(type_expr),
                            constraint: Box::new(constraint),
                        })
                    } else {
                        self.expect(TokenType::RightBrace)?;
                        Ok(Expr::ImplicitSet {
                            variable: var_name,
                            type_expr: Box::new(type_expr),
                        })
                    }
                } else {
                    // It's an explicit set or range set, need to backtrack
                    let first_elem = Expr::Identifier(var_name);
                    self.parse_explicit_or_range_set(first_elem)
                }
            }
            _ => {
                // Parse as explicit or range set
                let first_elem = self.parse_expression()?;
                self.parse_explicit_or_range_set(first_elem)
            }
        }
    }

    /// Parse explicit set { 1, 2, 3 } or range set { 0, 1, ..., 5 }
    fn parse_explicit_or_range_set(&mut self, first_elem: Expr) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Comma,
                ..
            }) => {
                self.advance(); // consume ,
                
                // Check if next is ellipsis (implicit step range)
                if let Some(Token {
                    token_type: TokenType::Ellipsis,
                    ..
                }) = self.peek()
                {
                    self.advance(); // consume ...
                    
                    // Might have comma before end
                    if let Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    }) = self.peek()
                    {
                        self.advance();
                    }
                    
                    let end_elem = self.parse_expression()?;
                    self.expect(TokenType::RightBrace)?;
                    
                    return Ok(Expr::RangeSet {
                        start: Box::new(first_elem),
                        step: None,
                        end: Box::new(end_elem),
                    });
                }
                
                // Otherwise, parse the second element
                let second_elem = self.parse_expression()?;
                
                match self.peek() {
                    Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    }) => {
                        self.advance(); // consume ,
                        
                        // Check for ...
                        if let Some(Token {
                            token_type: TokenType::Ellipsis,
                            ..
                        }) = self.peek()
                        {
                            self.advance(); // consume ...
                            
                            // Might have comma before end
                            if let Some(Token {
                                token_type: TokenType::Comma,
                                ..
                            }) = self.peek()
                            {
                                self.advance();
                            }
                            
                            let end_elem = self.parse_expression()?;
                            self.expect(TokenType::RightBrace)?;
                            
                            Ok(Expr::RangeSet {
                                start: Box::new(first_elem),
                                step: Some(Box::new(second_elem)),
                                end: Box::new(end_elem),
                            })
                        } else {
                            // Explicit set with multiple elements
                            let mut elements = vec![first_elem, second_elem];
                            loop {
                                elements.push(self.parse_expression()?);
                                
                                match self.peek() {
                                    Some(Token {
                                        token_type: TokenType::Comma,
                                        ..
                                    }) => {
                                        self.advance();
                                    }
                                    Some(Token {
                                        token_type: TokenType::RightBrace,
                                        ..
                                    }) => {
                                        self.advance();
                                        break;
                                    }
                                    _ => {
                                        return Err(ParseError::new(
                                            "Expected ',' or '}' in set",
                                        ))
                                    }
                                }
                            }
                            Ok(Expr::ExplicitSet(elements))
                        }
                    }
                    Some(Token {
                        token_type: TokenType::Ellipsis,
                        ..
                    }) => {
                        self.advance(); // consume ...
                        
                        // Might have comma before end
                        if let Some(Token {
                            token_type: TokenType::Comma,
                            ..
                        }) = self.peek()
                        {
                            self.advance();
                        }
                        
                        let end_elem = self.parse_expression()?;
                        self.expect(TokenType::RightBrace)?;
                        
                        Ok(Expr::RangeSet {
                            start: Box::new(first_elem),
                            step: None,
                            end: Box::new(end_elem),
                        })
                    }
                    Some(Token {
                        token_type: TokenType::RightBrace,
                        ..
                    }) => {
                        self.advance();
                        Ok(Expr::ExplicitSet(vec![first_elem, second_elem]))
                    }
                    _ => Err(ParseError::new("Unexpected token in set")),
                }
            }
            Some(Token {
                token_type: TokenType::RightBrace,
                ..
            }) => {
                self.advance();
                Ok(Expr::ExplicitSet(vec![first_elem]))
            }
            _ => Err(ParseError::new("Expected ',' or '}' in set")),
        }
    }
}
