package parser

import (
	"errors"
	"io"
	"math/big"

	"github.com/juliendoutre/jlang/pkg/sources"
	"github.com/juliendoutre/jlang/pkg/tokenizer"
)

func New(tokenizer *tokenizer.Tokenizer) *Parser {
	return &Parser{tokenizer: tokenizer}
}

type Parser struct {
	tokenizer *tokenizer.Tokenizer
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
	name, err := p.expectTokenWithType(tokenizer.IdentifierTokenType)
	if err != nil {
		return nil, err
	}

	if err := p.expectToken(tokenizer.SymbolTokenType, "="); err != nil {
		return nil, err
	}

	expression, err := p.parseExpression(nil)
	if err != nil {
		return nil, err
	}

	return &Assignement{
		Name:       name.Value,
		Expression: expression,
		Delimitation: sources.Delimitation{
			Start: name.Delimitation.Start,
			End:   expression.Delimit().End,
		},
	}, nil
}

func (p *Parser) parseSet(start sources.Location) (Set, error) {
	elements := []Expression{}

	token, err := p.tokenizer.Next()
	if err != nil {
		return nil, err
	}

	// Empty set
	if token.Type == tokenizer.SymbolTokenType && token.Value == "}" {
		return &EnumerationSet{
			Elements:     elements,
			Delimitation: sources.Delimitation{Start: start, End: token.Delimitation.End},
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

		if token.Type == tokenizer.SymbolTokenType && token.Value == "," {
			token = nil // force consuming a new token in the next expression parsing

			continue
		}

		if token.Type == tokenizer.SymbolTokenType && token.Value == "}" {
			return &EnumerationSet{
				Elements:     elements,
				Delimitation: sources.Delimitation{Start: start, End: token.Delimitation.End},
			}, nil
		}
	}
}

func (p *Parser) parseExpression(token *tokenizer.Token) (Expression, error) {
	var err error

	if token == nil {
		token, err = p.tokenizer.Next()
		if err != nil {
			return nil, err
		}
	}

	if token.Type == tokenizer.SymbolTokenType && token.Value == "{" {
		return p.parseSet(token.Delimitation.Start)
	}

	if token.Type == tokenizer.LiteralIntegerTokenType {
		intValue, ok := new(big.Int).SetString(token.Value, 0)
		if !ok {
			return nil, &sources.DelimitedError{Err: &InvalidIntegerLiteralError{raw: token.Value}, Delimitation: token.Delimitation}
		}

		return &LiteralInteger{Value: intValue, Delimitation: token.Delimitation}, nil
	}

	if token.Type == tokenizer.IdentifierTokenType {
		return &Identifier{Name: token.Value, Delimitation: token.Delimitation}, nil
	}

	return nil, &sources.DelimitedError{Err: &UnexpectedTokenTypesError{expected: []tokenizer.TokenType{tokenizer.LiteralIntegerTokenType, tokenizer.IdentifierTokenType}, actual: token.Type}, Delimitation: token.Delimitation}
}

func (p *Parser) expectTokenWithType(tokenType tokenizer.TokenType) (*tokenizer.Token, error) {
	token, err := p.tokenizer.Next()
	if err != nil {
		return nil, err
	}

	if token.Type != tokenType {
		return nil, &sources.DelimitedError{Err: &UnexpectedTokenTypesError{expected: []tokenizer.TokenType{tokenType}, actual: token.Type}, Delimitation: token.Delimitation}
	}

	return token, nil
}

func (p *Parser) expectToken(tokenType tokenizer.TokenType, value string) error {
	token, err := p.tokenizer.Next()
	if err != nil {
		return err
	}

	if token.Type != tokenType || token.Value != value {
		return &sources.DelimitedError{Err: &UnexpectedTokenError{
			expectedType:  tokenType,
			expectedValue: value,
			actualType:    token.Type,
			actualValue:   token.Value,
		}, Delimitation: token.Delimitation}
	}

	return nil
}
