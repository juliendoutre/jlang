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
	name, err := p.expectTokenWithType(IdentifierTokenType)
	if err != nil {
		return nil, err
	}

	if err := p.expectToken(SymbolTokenType, "="); err != nil {
		return nil, err
	}

	expression, err := p.parseExpression(nil)
	if err != nil {
		return nil, err
	}

	return &Assignement{
		Name:       name.Value,
		Expression: expression,
		Delimitation: Delimitation{
			Start: name.Delimitation.Start,
			End:   expression.Delimit().End,
		},
	}, nil
}

func (p *Parser) parseSet(start Location) (Set, error) {
	elements := []Expression{}

	token, err := p.tokenizer.Next()
	if err != nil {
		return nil, err
	}

	// Empty set
	if token.Type == SymbolTokenType && token.Value == "}" {
		return &EnumerationSet{
			Elements:     elements,
			Delimitation: Delimitation{Start: start, End: token.Delimitation.End},
		}, nil
	}

	for {
		expression, err := p.parseExpression(token)
		if err != nil {
			return nil, err
		}

		elements = append(elements, expression)

		token, err = p.tokenizer.Next()
		if err != nil {
			return nil, err
		}

		if token.Type == SymbolTokenType && token.Value == "," {
			token = nil // force consuming a new token in the next expression parsing

			continue
		}

		if token.Type == SymbolTokenType && token.Value == "}" {
			return &EnumerationSet{
				Elements:     elements,
				Delimitation: Delimitation{Start: start, End: token.Delimitation.End},
			}, nil
		}
	}
}

func (p *Parser) parseExpression(token *Token) (Expression, error) {
	var err error

	if token == nil {
		token, err = p.tokenizer.Next()
		if err != nil {
			return nil, err
		}
	}

	if token.Type == SymbolTokenType && token.Value == "{" {
		return p.parseSet(token.Delimitation.Start)
	}

	if token.Type == LiteralIntegerTokenType {
		intValue, ok := new(big.Int).SetString(token.Value, 0)
		if !ok {
			return nil, &DelimitedError{Err: &InvalidIntegerLiteralError{raw: token.Value}, Delimitation: token.Delimitation}
		}

		return &LiteralInteger{Value: intValue, Delimitation: token.Delimitation}, nil
	}

	if token.Type == IdentifierTokenType {
		return &Identifier{Name: token.Value, Delimitation: token.Delimitation}, nil
	}

	return nil, &DelimitedError{Err: &UnexpectedTokenTypesError{expected: []TokenType{LiteralIntegerTokenType, IdentifierTokenType}, actual: token.Type}, Delimitation: token.Delimitation}
}

func (p *Parser) expectTokenWithType(tokenType TokenType) (*Token, error) {
	token, err := p.tokenizer.Next()
	if err != nil {
		return nil, err
	}

	if token.Type != tokenType {
		return nil, &DelimitedError{Err: &UnexpectedTokenTypesError{expected: []TokenType{tokenType}, actual: token.Type}, Delimitation: token.Delimitation}
	}

	return token, nil
}

func (p *Parser) expectToken(tokenType TokenType, value string) error {
	token, err := p.tokenizer.Next()
	if err != nil {
		return err
	}

	if token.Type != tokenType || token.Value != value {
		return &DelimitedError{Err: &UnexpectedTokenError{
			expectedType:  tokenType,
			expectedValue: value,
			actualType:    token.Type,
			actualValue:   token.Value,
		}, Delimitation: token.Delimitation}
	}

	return nil
}
