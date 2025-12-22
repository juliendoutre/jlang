use crate::ast::{BinaryOperator, Expr, Parameter, Program, Statement, UnaryOperator};
use crate::error::{self, ParseError};
use crate::token::{Position, Token, TokenType};
use std::iter::Peekable;

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
    source: String,
    current_position: Position,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            source: String::new(),
            current_position: Position::new(1, 1),
        }
    }

    pub fn new_with_source(tokens: I, source: String) -> Self {
        Self {
            tokens: tokens.peekable(),
            source,
            current_position: Position::new(1, 1),
        }
    }

    /// Peek at the next token
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    /// Consume and return the next token
    fn advance(&mut self) -> Option<Token> {
        if let Some(token) = self.tokens.next() {
            self.current_position = token.position;
            Some(token)
        } else {
            None
        }
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
            Some(token)
                if std::mem::discriminant(&token.token_type)
                    == std::mem::discriminant(&expected) =>
            {
                Ok(token)
            }
            Some(token) => {
                let expected_str = format!("{:?}", expected);
                let found_str = format!("{:?}", token.token_type);
                let suggestion = error::suggest_for_unexpected_token(&expected_str, &found_str);

                Err(ParseError::new(format!("Expected {:?}, found {:?}", expected, token.token_type))
                    .with_position(token.position)
                    .with_source(self.source.clone())
                    .with_suggestion(suggestion))
            }
            None => Err(ParseError::new("Unexpected end of input")
                .with_position(self.current_position)
                .with_source(self.source.clone())
                .with_suggestion("File ended unexpectedly. Check for unclosed braces, parentheses, or brackets.")),
        }
    }

    /// Create an error at the current position
    fn error(&self, message: impl Into<String>) -> ParseError {
        ParseError::new(message)
            .with_position(self.current_position)
            .with_source(self.source.clone())
    }

    /// Create an error with a suggestion
    fn error_with_suggestion(&self, message: impl Into<String>, suggestion: impl Into<String>) -> ParseError {
        ParseError::new(message)
            .with_position(self.current_position)
            .with_source(self.source.clone())
            .with_suggestion(suggestion)
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
                TokenType::For => {
                    self.advance(); // consume 'for'

                    // Parse loop variable
                    let variable = match self.advance() {
                        Some(Token {
                            token_type: TokenType::Identifier(name),
                            ..
                        }) => name,
                        _ => return Err(self.error_with_suggestion(
                            "Expected identifier after 'for'",
                            "Loop variable must be an identifier, like: for i in ..."
                        )),
                    };

                    // Expect 'in'
                    match self.advance() {
                        Some(Token {
                            token_type: TokenType::In,
                            ..
                        }) => {}
                        _ => return Err(self.error_with_suggestion(
                            "Expected 'in' after loop variable",
                            "For loops use the syntax: for variable in iterable { ... }"
                        )),
                    }

                    // Parse iterable expression
                    let iterable = self.parse_expression()?;

                    // Expect '{'
                    self.skip_newlines();
                    self.expect(TokenType::LeftBrace)?;

                    // Parse body
                    let body = self.parse_function_body()?;

                    // Expect '}'
                    self.expect(TokenType::RightBrace)?;

                    Ok(Statement::For {
                        variable,
                        iterable,
                        body,
                    })
                }
                TokenType::If => {
                    self.advance(); // consume 'if'

                    // Parse condition
                    let condition = self.parse_expression()?;

                    // Expect '{'
                    self.skip_newlines();
                    self.expect(TokenType::LeftBrace)?;

                    // Parse body
                    let body = self.parse_function_body()?;

                    // Expect '}'
                    self.expect(TokenType::RightBrace)?;

                    Ok(Statement::If { condition, body })
                }
                TokenType::Identifier(name) => {
                    let name = name.clone();
                    self.advance();

                    // Check if this is a definition/assignment or a function call
                    match self.peek() {
                        Some(Token {
                            token_type: TokenType::LeftBracket,
                            ..
                        }) => {
                            // Could be array indexing: arr[i] = value
                            self.advance(); // consume [
                            let index = self.parse_expression()?;
                            self.expect(TokenType::RightBracket)?;

                            // Check if this is an assignment
                            if let Some(Token {
                                token_type: TokenType::Equals,
                                ..
                            }) = self.peek()
                            {
                                self.advance(); // consume =
                                let value = self.parse_expression()?;
                                Ok(Statement::IndexAssignment {
                                    array: name,
                                    index,
                                    value,
                                })
                            } else {
                                // Not an assignment, treat as expression statement
                                let expr = Expr::Index {
                                    array: Box::new(Expr::Identifier(name)),
                                    index: Box::new(index),
                                };
                                Ok(Statement::ExpressionStatement(expr))
                            }
                        }
                        Some(Token {
                            token_type: TokenType::Colon,
                            ..
                        }) => {
                            // Typed assignment: n: BYTE = in()
                            self.advance(); // consume :
                            let type_expr = self.parse_type_expr()?;
                            self.expect(TokenType::Equals)?;
                            let value = self.parse_expression()?;
                            Ok(Statement::TypedAssignment {
                                name,
                                type_expr,
                                value,
                            })
                        }
                        Some(Token {
                            token_type: TokenType::Equals,
                            ..
                        }) => {
                            self.advance(); // consume =

                            // Parse the value (could be expression, tuple, or function definition)
                            let value = self.parse_expression()?;

                            // Check if this might be a function definition by looking for ->
                            // If we see ->, try to parse as function definition
                            if matches!(
                                self.peek(),
                                Some(Token {
                                    token_type: TokenType::Arrow,
                                    ..
                                })
                            ) {
                                // This looks like a function definition
                                // The value we parsed should be a tuple literal representing parameters
                                // Parse the rest of the function definition
                                self.advance(); // consume ->
                                self.expect(TokenType::LeftParen)?;
                                let returns = self.parse_parameters()?;
                                self.expect(TokenType::RightParen)?;
                                self.expect(TokenType::LeftBrace)?;
                                let body = self.parse_function_body()?;
                                self.expect(TokenType::RightBrace)?;

                                // Extract parameters from the tuple literal
                                let params = match value {
                                    Expr::TupleLiteral { fields } => {
                                        // Convert tuple fields to parameters
                                        fields
                                            .into_iter()
                                            .map(|(name, type_expr)| Parameter {
                                                name,
                                                type_expr: Some(type_expr),
                                            })
                                            .collect()
                                    }
                        _ => {
                            return Err(self.error_with_suggestion(
                                "Function parameters must be a tuple",
                                "Define parameters like: (x: INTEGER, y: INTEGER)"
                            ));
                        }
                                };

                                Ok(Statement::FunctionDefinition {
                                    name,
                                    params,
                                    returns,
                                    body,
                                })
                            } else {
                                // Regular assignment or definition
                                if name.chars().next().unwrap().is_uppercase() {
                                    Ok(Statement::Definition { name, value })
                                } else {
                                    Ok(Statement::Assignment { name, value })
                                }
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

    /// Parse function parameters
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut params = Vec::new();

        // Check for empty parameters
        if let Some(Token {
            token_type: TokenType::RightParen,
            ..
        }) = self.peek()
        {
            return Ok(params);
        }

        loop {
            // Parse parameter name
            let name = match self.advance() {
                Some(Token {
                    token_type: TokenType::Identifier(n),
                    ..
                }) => n,
                _ => return Err(ParseError::new("Expected parameter name")),
            };

            // Check for type annotation
            let type_expr = if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = self.peek()
            {
                self.advance(); // consume :
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            params.push(Parameter { name, type_expr });

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

        Ok(params)
    }

    /// Parse a type expression (can include array types)
    fn parse_type_expr(&mut self) -> Result<Expr, ParseError> {
        if let Some(Token {
            token_type: TokenType::LeftBracket,
            ..
        }) = self.peek()
        {
            // Array type: [n]BYTE or [n: BYTE]BYTE or [2*n]BYTE
            self.advance(); // consume [

            // Check if this is a type-constrained size: [n: BYTE]
            let size = if let Some(Token {
                token_type: TokenType::Identifier(name),
                ..
            }) = self.peek()
            {
                let name = name.clone();
                self.advance(); // consume identifier

                // Check for colon (type constraint)
                if let Some(Token {
                    token_type: TokenType::Colon,
                    ..
                }) = self.peek()
                {
                    self.advance(); // consume :
                    let type_expr = self.parse_array_size_type_expr()?;
                    Expr::TypeConstrained {
                        name,
                        type_expr: Box::new(type_expr),
                    }
                } else {
                    // No colon, so it's just an identifier
                    // Put it back and parse as expression
                    // We need to parse the rest of the expression
                    let mut left = Expr::Identifier(name);

                    // Check for binary operators
                    while let Some(token) = self.peek() {
                        match &token.token_type {
                            TokenType::Plus
                            | TokenType::Minus
                            | TokenType::Star
                            | TokenType::Percent => {
                                let op = match &token.token_type {
                                    TokenType::Plus => BinaryOperator::Add,
                                    TokenType::Minus => BinaryOperator::Subtract,
                                    TokenType::Star => BinaryOperator::Multiply,
                                    TokenType::Percent => BinaryOperator::Modulo,
                                    _ => unreachable!(),
                                };
                                self.advance();
                                let right = self.parse_primary_expr()?;
                                left = Expr::BinaryOp {
                                    op,
                                    left: Box::new(left),
                                    right: Box::new(right),
                                };
                            }
                            TokenType::RightBracket => break,
                            _ => break,
                        }
                    }

                    left
                }
            } else {
                // Not an identifier, parse as regular expression
                self.parse_array_size_expr()?
            };

            self.expect(TokenType::RightBracket)?;
            let element_type = self.parse_type_expr()?;
            Ok(Expr::ArrayType {
                size: Box::new(size),
                element_type: Box::new(element_type),
            })
        } else {
            self.parse_expression()
        }
    }

    /// Parse an expression inside array size brackets (limited context)
    fn parse_array_size_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_primary_expr()?;

        // Handle binary operators in array size context
        while let Some(token) = self.peek() {
            match &token.token_type {
                TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Percent => {
                    let op = match &token.token_type {
                        TokenType::Plus => BinaryOperator::Add,
                        TokenType::Minus => BinaryOperator::Subtract,
                        TokenType::Star => BinaryOperator::Multiply,
                        TokenType::Percent => BinaryOperator::Modulo,
                        _ => unreachable!(),
                    };
                    self.advance();
                    let right = self.parse_primary_expr()?;
                    left = Expr::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                }
                TokenType::RightBracket => break,
                _ => break,
            }
        }

        Ok(left)
    }

    /// Parse a type expression in array size context (after colon)
    fn parse_array_size_type_expr(&mut self) -> Result<Expr, ParseError> {
        // In [n: BYTE] context, just parse identifier or set
        match self.peek() {
            Some(Token {
                token_type: TokenType::Identifier(name),
                ..
            }) => {
                let name = name.clone();
                self.advance();
                Ok(Expr::Identifier(name))
            }
            Some(Token {
                token_type: TokenType::LeftBrace,
                ..
            }) => {
                self.advance();
                self.parse_set_expr()
            }
            _ => Err(ParseError::new(
                "Expected type expression after ':'".to_string(),
            )),
        }
    }

    /// Parse function body (for now, just collect statements until closing brace)
    fn parse_function_body(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut body = Vec::new();
        self.skip_newlines();

        while let Some(token) = self.peek() {
            if matches!(token.token_type, TokenType::RightBrace) {
                break;
            }

            // Parse statements - try normal statement parsing first,
            // but if it starts with a non-identifier/non-keyword, treat as expression statement
            let stmt = if matches!(
                token.token_type,
                TokenType::Identifier(_) | TokenType::For | TokenType::If
            ) {
                self.parse_statement()?
            } else {
                // Treat as expression statement
                let expr = self.parse_expression()?;
                Statement::ExpressionStatement(expr)
            };

            body.push(stmt);
            self.skip_newlines();
        }

        Ok(body)
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
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_expr(0)
    }

    /// Parse binary expressions with precedence
    fn parse_binary_expr(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary_expr()?;

        while let Some(token) = self.peek() {
            let (op, precedence) = match &token.token_type {
                TokenType::Minus => (BinaryOperator::Subtract, 1),
                TokenType::Plus => (BinaryOperator::Add, 1),
                TokenType::Star => (BinaryOperator::Multiply, 2),
                TokenType::Slash => (BinaryOperator::Divide, 2),
                TokenType::Percent => (BinaryOperator::Modulo, 2),
                TokenType::Caret => (BinaryOperator::Power, 3), // Higher precedence for power
                TokenType::DoubleEquals => (BinaryOperator::Equals, 0),
                TokenType::LessThan => (BinaryOperator::LessThan, 0),
                TokenType::GreaterThan => (BinaryOperator::GreaterThan, 0),
                TokenType::LessThanOrEqual => (BinaryOperator::LessThanOrEqual, 0),
                TokenType::GreaterThanOrEqual => (BinaryOperator::GreaterThanOrEqual, 0),
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

    /// Parse unary expressions (like -x)
    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Minus,
                ..
            }) => {
                self.advance(); // consume -
                let operand = self.parse_unary_expr()?; // Allow chaining: --x
                Ok(Expr::UnaryOp {
                    op: UnaryOperator::Negate,
                    operand: Box::new(operand),
                })
            }
            _ => self.parse_postfix_expr(),
        }
    }

    /// Parse postfix expressions (array indexing, slicing, field access, etc.)
    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            match self.peek() {
                Some(Token {
                    token_type: TokenType::LeftBracket,
                    ..
                }) => {
                    self.advance(); // consume [

                    // Check for slice starting with colon: array[:]
                    if let Some(Token {
                        token_type: TokenType::Colon,
                        ..
                    }) = self.peek()
                    {
                        self.advance(); // consume :

                        // Check if there's an end expression
                        let end = if matches!(
                            self.peek(),
                            Some(Token {
                                token_type: TokenType::RightBracket,
                                ..
                            })
                        ) {
                            None
                        } else {
                            Some(Box::new(self.parse_expression()?))
                        };

                        self.expect(TokenType::RightBracket)?;
                        expr = Expr::Slice {
                            array: Box::new(expr),
                            start: None,
                            end,
                        };
                    } else {
                        // Parse first expression (could be index or slice start)
                        let first = self.parse_expression()?;

                        // Check for colon (slice)
                        if let Some(Token {
                            token_type: TokenType::Colon,
                            ..
                        }) = self.peek()
                        {
                            self.advance(); // consume :

                            // Check if there's an end expression
                            let end = if matches!(
                                self.peek(),
                                Some(Token {
                                    token_type: TokenType::RightBracket,
                                    ..
                                })
                            ) {
                                None
                            } else {
                                Some(Box::new(self.parse_expression()?))
                            };

                            self.expect(TokenType::RightBracket)?;
                            expr = Expr::Slice {
                                array: Box::new(expr),
                                start: Some(Box::new(first)),
                                end,
                            };
                        } else {
                            // Regular indexing
                            self.expect(TokenType::RightBracket)?;
                            expr = Expr::Index {
                                array: Box::new(expr),
                                index: Box::new(first),
                            };
                        }
                    }
                }
                Some(Token {
                    token_type: TokenType::Dot,
                    ..
                }) => {
                    self.advance(); // consume .

                    // Expect field name
                    let field = match self.advance() {
                        Some(Token {
                            token_type: TokenType::Identifier(name),
                            ..
                        }) => name,
                        _ => return Err(ParseError::new("Expected field name after '.'")),
                    };

                    expr = Expr::FieldAccess {
                        object: Box::new(expr),
                        field,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    /// Parse primary expressions
    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.advance() {
            Some(Token {
                token_type: TokenType::Integer(n),
                ..
            }) => Ok(Expr::Integer(n)),
            Some(Token {
                token_type: TokenType::Character(c),
                ..
            }) => Ok(Expr::Character(c)),
            Some(Token {
                token_type: TokenType::LeftParen,
                ..
            }) => {
                // Could be a parenthesized expression or tuple literal
                // We need to look ahead to distinguish them

                // Check for empty tuple/parens
                if let Some(Token {
                    token_type: TokenType::RightParen,
                    ..
                }) = self.peek()
                {
                    self.advance();
                    return Ok(Expr::TupleLiteral { fields: Vec::new() });
                }

                // Check if this looks like a tuple: identifier followed by colon
                // We need to peek ahead to see if there's a pattern like "id:"
                let is_tuple = if let Some(Token {
                    token_type: TokenType::Identifier(_),
                    ..
                }) = self.peek()
                {
                    // We need to check if the next token after the identifier is a colon
                    // Since we can't easily peek 2 tokens ahead, we'll parse the identifier
                    // and check, then decide what to do
                    true // tentatively assume it might be a tuple
                } else {
                    false
                };

                if is_tuple {
                    // Parse first identifier
                    let first_id = match self.advance() {
                        Some(Token {
                            token_type: TokenType::Identifier(name),
                            ..
                        }) => name,
                        _ => unreachable!(),
                    };

                    // Check if followed by colon
                    if matches!(
                        self.peek(),
                        Some(Token {
                            token_type: TokenType::Colon,
                            ..
                        })
                    ) {
                        // It's a tuple! Parse it as such
                        self.advance(); // consume :
                        let first_value = self.parse_expression()?;
                        let mut fields = vec![(first_id, first_value)];

                        // Parse remaining fields
                        loop {
                            match self.peek() {
                                Some(Token {
                                    token_type: TokenType::Comma,
                                    ..
                                }) => {
                                    self.advance();
                                    // Allow trailing comma
                                    if matches!(
                                        self.peek(),
                                        Some(Token {
                                            token_type: TokenType::RightParen,
                                            ..
                                        })
                                    ) {
                                        break;
                                    }
                                    // Parse next field
                                    let field_name = match self.advance() {
                                        Some(Token {
                                            token_type: TokenType::Identifier(name),
                                            ..
                                        }) => name,
                                        _ => {
                                            return Err(ParseError::new(
                                                "Expected field name in tuple",
                                            ));
                                        }
                                    };
                                    self.expect(TokenType::Colon)?;
                                    let field_value = self.parse_expression()?;
                                    fields.push((field_name, field_value));
                                }
                                Some(Token {
                                    token_type: TokenType::RightParen,
                                    ..
                                }) => break,
                                _ => return Err(ParseError::new("Expected ',' or ')' in tuple")),
                            }
                        }

                        self.expect(TokenType::RightParen)?;
                        Ok(Expr::TupleLiteral { fields })
                    } else {
                        // Not a tuple, it's a parenthesized expression starting with an identifier
                        // We've already consumed the identifier, so we need to build an expression from it
                        let mut expr = Expr::Identifier(first_id);

                        // Continue parsing as a binary expression if there are operators
                        // We need to handle the rest of the expression inside the parens
                        loop {
                            match self.peek() {
                                Some(Token {
                                    token_type: TokenType::RightParen,
                                    ..
                                }) => {
                                    self.advance();
                                    return Ok(expr);
                                }
                                Some(Token { token_type, .. })
                                    if matches!(
                                        token_type,
                                        TokenType::Plus
                                            | TokenType::Minus
                                            | TokenType::Star
                                            | TokenType::Slash
                                            | TokenType::Percent
                                            | TokenType::DoubleEquals
                                            | TokenType::LessThan
                                            | TokenType::GreaterThan
                                            | TokenType::LessThanOrEqual
                                            | TokenType::GreaterThanOrEqual
                                            | TokenType::Ampersand
                                    ) =>
                                {
                                    // Parse as binary expression
                                    // We need to continue parsing the full expression
                                    // This is tricky because we've already parsed the left side
                                    // Let's just parse the rest as a binary expression
                                    let (op, _) = match token_type {
                                        TokenType::Plus => (BinaryOperator::Add, 1),
                                        TokenType::Minus => (BinaryOperator::Subtract, 1),
                                        TokenType::Star => (BinaryOperator::Multiply, 2),
                                        TokenType::Slash => (BinaryOperator::Divide, 2),
                                        TokenType::Percent => (BinaryOperator::Modulo, 2),
                                        TokenType::DoubleEquals => (BinaryOperator::Equals, 0),
                                        TokenType::LessThan => (BinaryOperator::LessThan, 0),
                                        TokenType::GreaterThan => (BinaryOperator::GreaterThan, 0),
                                        TokenType::LessThanOrEqual => {
                                            (BinaryOperator::LessThanOrEqual, 0)
                                        }
                                        TokenType::GreaterThanOrEqual => {
                                            (BinaryOperator::GreaterThanOrEqual, 0)
                                        }
                                        TokenType::Ampersand => (BinaryOperator::And, 0),
                                        _ => unreachable!(),
                                    };
                                    self.advance(); // consume operator
                                    let right = self.parse_postfix_expr()?;
                                    expr = Expr::BinaryOp {
                                        op,
                                        left: Box::new(expr),
                                        right: Box::new(right),
                                    };
                                }
                                _ => {
                                    return Err(ParseError::new(
                                        "Unexpected token in parenthesized expression",
                                    ));
                                }
                            }
                        }
                    }
                } else {
                    // Not starting with identifier, parse as regular parenthesized expression
                    let expr = self.parse_expression()?;
                    self.expect(TokenType::RightParen)?;
                    Ok(expr)
                }
            }
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
            Some(Token {
                token_type: TokenType::LeftBracket,
                ..
            }) => {
                // [ is already consumed by the match

                // Check for empty array literal
                if let Some(Token {
                    token_type: TokenType::RightBracket,
                    ..
                }) = self.peek()
                {
                    self.advance();
                    return Ok(Expr::ArrayLiteral(Vec::new()));
                }

                // Parse first element (might be size for array type or first element of literal)
                let first = self.parse_expression()?;

                // Check what comes next
                if let Some(Token {
                    token_type: TokenType::RightBracket,
                    ..
                }) = self.peek()
                {
                    self.advance(); // consume ]
                    // Check if this is array type: [size]Element
                    if let Some(Token {
                        token_type: TokenType::Identifier(_) | TokenType::LeftBracket,
                        ..
                    }) = self.peek()
                    {
                        let element_type = self.parse_type_expr()?;
                        return Ok(Expr::ArrayType {
                            size: Box::new(first),
                            element_type: Box::new(element_type),
                        });
                    } else {
                        // Single-element array literal
                        return Ok(Expr::ArrayLiteral(vec![first]));
                    }
                }

                // Must be array literal with multiple elements: [a, b, c]
                // Or array range: [0, 1, ..., n]
                if !matches!(
                    self.peek(),
                    Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    })
                ) {
                    return Err(ParseError::new("Expected ',' or ']' in array"));
                }

                self.advance(); // consume comma

                // Check for range pattern: [start, ...] or [start, step, ...]
                if let Some(Token {
                    token_type: TokenType::Ellipsis,
                    ..
                }) = self.peek()
                {
                    // Range with implicit step: [0, ..., 10]
                    self.advance(); // consume ...

                    // Optional comma before end
                    if let Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    }) = self.peek()
                    {
                        self.advance();
                    }

                    let end_elem = self.parse_expression()?;
                    self.expect(TokenType::RightBracket)?;

                    return Ok(Expr::ArrayRange {
                        start: Box::new(first),
                        step: None,
                        end: Box::new(end_elem),
                    });
                }

                // Parse second element
                let second = self.parse_expression()?;

                // Check what comes after second element
                match self.peek() {
                    Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    }) => {
                        self.advance(); // consume comma

                        // Check for ellipsis (explicit step range)
                        if let Some(Token {
                            token_type: TokenType::Ellipsis,
                            ..
                        }) = self.peek()
                        {
                            self.advance(); // consume ...

                            // Optional comma before end
                            if let Some(Token {
                                token_type: TokenType::Comma,
                                ..
                            }) = self.peek()
                            {
                                self.advance();
                            }

                            let end_elem = self.parse_expression()?;
                            self.expect(TokenType::RightBracket)?;

                            return Ok(Expr::ArrayRange {
                                start: Box::new(first),
                                step: Some(Box::new(second)),
                                end: Box::new(end_elem),
                            });
                        }

                        // Not a range, continue parsing as regular array literal
                        let mut elements = vec![first, second];
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
                                    token_type: TokenType::RightBracket,
                                    ..
                                }) => {
                                    self.advance();
                                    break;
                                }
                                _ => return Err(ParseError::new("Expected ',' or ']' in array")),
                            }
                        }
                        Ok(Expr::ArrayLiteral(elements))
                    }
                    Some(Token {
                        token_type: TokenType::RightBracket,
                        ..
                    }) => {
                        self.advance();
                        Ok(Expr::ArrayLiteral(vec![first, second]))
                    }
                    _ => Err(ParseError::new("Expected ',' or ']' in array")),
                }
            }
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

        // Check for tuple type: { (x: T, y: U, ...) }
        if let Some(Token {
            token_type: TokenType::LeftParen,
            ..
        }) = self.peek()
        {
            self.advance(); // consume (
            let fields = self.parse_tuple_fields()?;
            self.expect(TokenType::RightParen)?;
            self.expect(TokenType::RightBrace)?;
            return Ok(Expr::ExplicitSet(vec![Expr::TupleType { fields }]));
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

                // Check for : (implicit set with optional constraint)
                if let Some(Token {
                    token_type: TokenType::Colon,
                    ..
                }) = self.peek()
                {
                    self.advance(); // consume :
                    let type_expr = self.parse_expression()?;

                    // Check for | (constraint)
                    let constraint = if let Some(Token {
                        token_type: TokenType::Pipe,
                        ..
                    }) = self.peek()
                    {
                        self.advance(); // consume |
                        Some(Box::new(self.parse_expression()?))
                    } else {
                        None
                    };

                    self.expect(TokenType::RightBrace)?;
                    Ok(Expr::ImplicitSet {
                        variable: var_name,
                        type_expr: Box::new(type_expr),
                        constraint,
                    })
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
                                    _ => return Err(self.error_with_suggestion(
                                        "Expected ',' or '}' in set",
                                        "Sets use the syntax: {1, 2, 3} or {0, ..., 10}. Make sure to close all braces."
                                    )),
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

    /// Parse tuple fields: field: type, field: type, ...
    fn parse_tuple_fields(&mut self) -> Result<Vec<(String, Expr)>, ParseError> {
        let mut fields = Vec::new();

        // Check for empty tuple
        if let Some(Token {
            token_type: TokenType::RightParen,
            ..
        }) = self.peek()
        {
            return Ok(fields);
        }

        loop {
            // Parse field name
            let field_name = match self.advance() {
                Some(Token {
                    token_type: TokenType::Identifier(name),
                    ..
                }) => name,
                _ => return Err(ParseError::new("Expected field name in tuple")),
            };

            // Expect :
            self.expect(TokenType::Colon)?;

            // Parse field type or value
            let field_value = self.parse_expression()?;

            fields.push((field_name, field_value));

            // Check for comma or end
            match self.peek() {
                Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) => {
                    self.advance();
                    // Allow trailing comma
                    if matches!(
                        self.peek(),
                        Some(Token {
                            token_type: TokenType::RightParen,
                            ..
                        })
                    ) {
                        break;
                    }
                }
                Some(Token {
                    token_type: TokenType::RightParen,
                    ..
                }) => break,
                _ => return Err(ParseError::new("Expected ',' or ')' in tuple")),
            }
        }

        Ok(fields)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse_expr(input: &str) -> Result<Expr, ParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new_with_source(lexer, input.to_string());
        parser.parse_expression()
    }

    fn parse(input: &str) -> Result<Program, ParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new_with_source(lexer, input.to_string());
        parser.parse()
    }

    #[test]
    fn test_parse_integer() {
        let result = parse_expr("42").unwrap();
        assert_eq!(result, Expr::Integer(42));
    }

    #[test]
    fn test_parse_character() {
        let result = parse_expr("'a'").unwrap();
        assert_eq!(result, Expr::Character('a'));
    }

    #[test]
    fn test_parse_identifier() {
        let result = parse_expr("foo").unwrap();
        assert_eq!(result, Expr::Identifier("foo".to_string()));
    }

    #[test]
    fn test_parse_binary_addition() {
        let result = parse_expr("1 + 2").unwrap();
        match result {
            Expr::BinaryOp {
                op,
                left,
                right,
            } => {
                assert_eq!(op, BinaryOperator::Add);
                assert_eq!(*left, Expr::Integer(1));
                assert_eq!(*right, Expr::Integer(2));
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_parse_binary_precedence() {
        let result = parse_expr("2 + 3 * 4").unwrap();
        match result {
            Expr::BinaryOp {
                op: BinaryOperator::Add,
                left,
                right,
            } => {
                assert_eq!(*left, Expr::Integer(2));
                match *right {
                    Expr::BinaryOp {
                        op: BinaryOperator::Multiply,
                        left: mult_left,
                        right: mult_right,
                    } => {
                        assert_eq!(*mult_left, Expr::Integer(3));
                        assert_eq!(*mult_right, Expr::Integer(4));
                    }
                    _ => panic!("Expected multiply operation on right side"),
                }
            }
            _ => panic!("Expected addition at top level"),
        }
    }

    #[test]
    fn test_parse_unary_negation() {
        let result = parse_expr("-5").unwrap();
        match result {
            Expr::UnaryOp { op, operand } => {
                assert_eq!(op, UnaryOperator::Negate);
                assert_eq!(*operand, Expr::Integer(5));
            }
            _ => panic!("Expected UnaryOp"),
        }
    }

    #[test]
    fn test_parse_parenthesized_expr() {
        let result = parse_expr("(2 + 3)").unwrap();
        match result {
            Expr::BinaryOp { op, left, right } => {
                assert_eq!(op, BinaryOperator::Add);
                assert_eq!(*left, Expr::Integer(2));
                assert_eq!(*right, Expr::Integer(3));
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let result = parse_expr("foo(1, 2)").unwrap();
        match result {
            Expr::FunctionCall { name, args } => {
                assert_eq!(name, "foo");
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], Expr::Integer(1));
                assert_eq!(args[1], Expr::Integer(2));
            }
            _ => panic!("Expected FunctionCall"),
        }
    }

    #[test]
    fn test_parse_function_call_no_args() {
        let result = parse_expr("stdin()").unwrap();
        match result {
            Expr::FunctionCall { name, args } => {
                assert_eq!(name, "stdin");
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected FunctionCall"),
        }
    }

    #[test]
    fn test_parse_array_literal() {
        let result = parse_expr("[1, 2, 3]").unwrap();
        match result {
            Expr::ArrayLiteral(elements) => {
                assert_eq!(elements.len(), 3);
                assert_eq!(elements[0], Expr::Integer(1));
                assert_eq!(elements[1], Expr::Integer(2));
                assert_eq!(elements[2], Expr::Integer(3));
            }
            _ => panic!("Expected ArrayLiteral"),
        }
    }

    #[test]
    fn test_parse_empty_array() {
        let result = parse_expr("[]").unwrap();
        match result {
            Expr::ArrayLiteral(elements) => {
                assert_eq!(elements.len(), 0);
            }
            _ => panic!("Expected ArrayLiteral"),
        }
    }

    #[test]
    fn test_parse_array_range() {
        let result = parse_expr("[0, ..., 10]").unwrap();
        match result {
            Expr::ArrayRange { start, step, end } => {
                assert_eq!(*start, Expr::Integer(0));
                assert!(step.is_none());
                assert_eq!(*end, Expr::Integer(10));
            }
            _ => panic!("Expected ArrayRange"),
        }
    }

    #[test]
    fn test_parse_array_index() {
        let result = parse_expr("arr[5]").unwrap();
        match result {
            Expr::Index { array, index } => {
                assert_eq!(*array, Expr::Identifier("arr".to_string()));
                assert_eq!(*index, Expr::Integer(5));
            }
            _ => panic!("Expected Index"),
        }
    }

    #[test]
    fn test_parse_array_slice() {
        let result = parse_expr("arr[1:5]").unwrap();
        match result {
            Expr::Slice { array, start, end } => {
                assert_eq!(*array, Expr::Identifier("arr".to_string()));
                assert_eq!(*start.clone().unwrap(), Expr::Integer(1));
                assert_eq!(*end.clone().unwrap(), Expr::Integer(5));
            }
            _ => panic!("Expected Slice"),
        }
    }

    #[test]
    fn test_parse_explicit_set() {
        let result = parse_expr("{1, 2, 3}").unwrap();
        match result {
            Expr::ExplicitSet(elements) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected ExplicitSet"),
        }
    }

    #[test]
    fn test_parse_range_set() {
        let result = parse_expr("{0, ..., 10}").unwrap();
        match result {
            Expr::RangeSet { start, step, end } => {
                assert_eq!(*start, Expr::Integer(0));
                assert!(step.is_none());
                assert_eq!(*end, Expr::Integer(10));
            }
            _ => panic!("Expected RangeSet"),
        }
    }

    #[test]
    fn test_parse_tuple_literal() {
        let result = parse_expr("(x: 1, y: 2)").unwrap();
        match result {
            Expr::TupleLiteral { fields } => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "x");
                assert_eq!(fields[0].1, Expr::Integer(1));
                assert_eq!(fields[1].0, "y");
                assert_eq!(fields[1].1, Expr::Integer(2));
            }
            _ => panic!("Expected TupleLiteral"),
        }
    }

    #[test]
    fn test_parse_field_access() {
        let result = parse_expr("tuple.field").unwrap();
        match result {
            Expr::FieldAccess { object, field } => {
                assert_eq!(*object, Expr::Identifier("tuple".to_string()));
                assert_eq!(field, "field");
            }
            _ => panic!("Expected FieldAccess"),
        }
    }

    #[test]
    fn test_parse_assignment() {
        let result = parse("x = 42").unwrap();
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment { name, value } => {
                assert_eq!(name, "x");
                assert_eq!(*value, Expr::Integer(42));
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_parse_definition() {
        let result = parse("X = {1, 2, 3}").unwrap();
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Definition { name, .. } => {
                assert_eq!(name, "X");
            }
            _ => panic!("Expected Definition"),
        }
    }

    #[test]
    fn test_parse_typed_assignment() {
        let result = parse("n: BYTE = stdin()").unwrap();
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::TypedAssignment { name, .. } => {
                assert_eq!(name, "n");
            }
            _ => panic!("Expected TypedAssignment"),
        }
    }

    #[test]
    fn test_parse_if_statement() {
        let result = parse("if x == 0 { y = 1 }").unwrap();
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::If { condition, body } => {
                match condition {
                    Expr::BinaryOp { op, .. } => {
                        assert_eq!(*op, BinaryOperator::Equals);
                    }
                    _ => panic!("Expected BinaryOp in condition"),
                }
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected If"),
        }
    }

    #[test]
    fn test_parse_for_loop() {
        let result = parse("for i in [0, 1, 2] { x = i }").unwrap();
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::For {
                variable,
                iterable,
                body,
            } => {
                assert_eq!(variable, "i");
                match iterable {
                    Expr::ArrayLiteral(_) => {}
                    _ => panic!("Expected ArrayLiteral"),
                }
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected For"),
        }
    }

    #[test]
    fn test_parse_function_definition() {
        let input = "add = (x: INTEGER, y: INTEGER) -> (result: INTEGER) { result = x + y }";
        let result = parse(input).unwrap();
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::FunctionDefinition {
                name,
                params,
                returns,
                body,
            } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert_eq!(returns.len(), 1);
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected FunctionDefinition"),
        }
    }

    #[test]
    fn test_parse_multiple_statements() {
        let input = "x = 1\ny = 2\nz = x + y";
        let result = parse(input).unwrap();
        assert_eq!(result.statements.len(), 3);
    }

    #[test]
    fn test_parse_comparisons() {
        assert!(parse_expr("a < b").is_ok());
        assert!(parse_expr("a > b").is_ok());
        assert!(parse_expr("a <= b").is_ok());
        assert!(parse_expr("a >= b").is_ok());
        assert!(parse_expr("a == b").is_ok());
    }

    #[test]
    fn test_parse_power() {
        let result = parse_expr("2 ^ 3").unwrap();
        match result {
            Expr::BinaryOp {
                op: BinaryOperator::Power,
                left,
                right,
            } => {
                assert_eq!(*left, Expr::Integer(2));
                assert_eq!(*right, Expr::Integer(3));
            }
            _ => panic!("Expected power operation"),
        }
    }

    #[test]
    fn test_parse_modulo() {
        let result = parse_expr("10 % 3").unwrap();
        match result {
            Expr::BinaryOp {
                op: BinaryOperator::Modulo,
                ..
            } => {}
            _ => panic!("Expected modulo operation"),
        }
    }

    #[test]
    fn test_parse_error_unexpected_eof() {
        let result = parse_expr("1 +");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_empty_program() {
        let result = parse("").unwrap();
        assert_eq!(result.statements.len(), 0);
    }

    #[test]
    fn test_parse_with_newlines() {
        let input = "\n\nx = 1\n\n\ny = 2\n\n";
        let result = parse(input).unwrap();
        assert_eq!(result.statements.len(), 2);
    }
}
