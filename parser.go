package jlang

import (
	"errors"
	"fmt"
	"io"
	"math/big"
	"strings"
)

type UnexpectedTokenError struct {
	actualType, expectedType   TokenType
	actualValue, expectedValue string
}

func (u *UnexpectedTokenError) Error() string {
	return fmt.Sprintf("expected a %s with value %q but read a %s with value %q", u.expectedType, u.expectedValue, u.actualType, u.actualValue)
}

type UnexpectedTokenTypesError struct {
	actual   TokenType
	expected []TokenType
}

func (u *UnexpectedTokenTypesError) Error() string {
	expectedString := make([]string, 0, len(u.expected))
	for _, expected := range u.expected {
		expectedString = append(expectedString, expected.String())
	}

	return fmt.Sprintf("expected one of %s but read a %s", strings.Join(expectedString, " or "), u.actual)
}

type InvalidIntegerLiteralError struct {
	raw string
}

func (i *InvalidIntegerLiteralError) Error() string {
	return fmt.Sprintf("failed parsing literal %q as an integer", i.raw)
}

func NewParser(tokenizer *Tokenizer) *Parser {
	return &Parser{tokenizer: tokenizer}
}

type Parser struct {
	tokenizer *Tokenizer
}

func (p *Parser) Parse() (*AST, error) {
	ast := &AST{Statements: []Statement{}}

	for {
		statement, err := p.parseStatement()
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}

			return nil, err
		}

		ast.Statements = append(ast.Statements, statement)
	}

	return ast, nil
}

func (p *Parser) parseStatement() (Statement, error) {
	return p.parseAssignement()
}

func (p *Parser) parseAssignement() (Assignement, error) {
	identifier, err := p.expectTokenWithType(IdentifierTokenType)
	if err != nil {
		return Assignement{}, err
	}

	if err := p.expectToken(SymbolTokenType, "="); err != nil {
		return Assignement{}, err
	}

	expression, err := p.parseExpression()
	if err != nil {
		return Assignement{}, err
	}

	return Assignement{
		Name:       identifier,
		Expression: expression,
	}, nil
}

func (p *Parser) parseExpression() (Expression, error) {
	token, err := p.tokenizer.Next()
	if err != nil {
		return nil, err
	}

	if token.Type == LiteralIntegerTokenType {
		intValue, ok := new(big.Int).SetString(token.Value, 0)
		if !ok {
			return nil, &InvalidIntegerLiteralError{raw: token.Value}
		}

		return LiteralInteger{Value: intValue}, nil
	}

	if token.Type == IdentifierTokenType {
		return Identifier{Name: token.Value}, nil
	}

	return nil, &UnexpectedTokenTypesError{expected: []TokenType{LiteralIntegerTokenType, IdentifierTokenType}, actual: token.Type}
}

func (p *Parser) expectTokenWithType(tokenType TokenType) (string, error) {
	token, err := p.tokenizer.Next()
	if err != nil {
		return "", err
	}

	if token.Type != tokenType {
		return "", &UnexpectedTokenTypesError{expected: []TokenType{tokenType}, actual: token.Type}
	}

	return token.Value, nil
}

func (p *Parser) expectToken(tokenType TokenType, value string) error {
	token, err := p.tokenizer.Next()
	if err != nil {
		return err
	}

	if token.Type != tokenType || token.Value != value {
		return &UnexpectedTokenError{
			expectedType:  tokenType,
			expectedValue: value,
			actualType:    token.Type,
			actualValue:   token.Value,
		}
	}

	return nil
}
