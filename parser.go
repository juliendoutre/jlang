package jlang

import (
	"errors"
	"fmt"
	"io"
	"math/big"
)

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
			return nil, fmt.Errorf("failed parsing literal integer from %s", token.Value)
		}

		return LiteralInteger{Value: intValue}, nil
	}

	if token.Type == IdentifierTokenType {
		return Identifier{Name: token.Value}, nil
	}

	return nil, fmt.Errorf("expected a %s or an %s but read a %s", LiteralIntegerTokenType, IdentifierTokenType, token.Type)
}

func (p *Parser) expectTokenWithType(tokenType TokenType) (string, error) {
	token, err := p.tokenizer.Next()
	if err != nil {
		return "", err
	}

	if token.Type != tokenType {
		return "", fmt.Errorf("expected a %s but read a %s", tokenType, token.Type)
	}

	return token.Value, nil
}

func (p *Parser) expectToken(tokenType TokenType, value string) error {
	token, err := p.tokenizer.Next()
	if err != nil {
		return err
	}

	if token.Type != tokenType || token.Value != value {
		return fmt.Errorf("expected a %s with value %q but read a %s with value %q", tokenType, value, token.Type, token.Value)
	}

	return nil
}
