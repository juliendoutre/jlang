package parser_test

import (
	"math/big"
	"strings"
	"testing"

	"github.com/juliendoutre/jlang/pkg/parser"
	"github.com/juliendoutre/jlang/pkg/tokenizer"
	"github.com/stretchr/testify/require"
)

func TestParser(t *testing.T) {
	t.Parallel()

	for testCase, expectedAST := range map[string]*parser.AST{
		"":    {Statements: []parser.Statement{}},
		"   ": {Statements: []parser.Statement{}},
		"a = 10": {Statements: []parser.Statement{
			&parser.Assignement{
				Name:       "a",
				Expression: &parser.LiteralInteger{Value: big.NewInt(10)},
			},
		}},
		"a = 10\nb = a": {Statements: []parser.Statement{
			&parser.Assignement{
				Name:       "a",
				Expression: &parser.LiteralInteger{Value: big.NewInt(10)},
			},
			&parser.Assignement{
				Name:       "b",
				Expression: &parser.Identifier{Name: "a"},
			},
		}},
		"b = a": {Statements: []parser.Statement{
			&parser.Assignement{
				Name:       "b",
				Expression: &parser.Identifier{Name: "a"},
			},
		}},
		"EMPTY = {}": {Statements: []parser.Statement{
			&parser.Assignement{
				Name:       "EMPTY",
				Expression: &parser.EnumerationSet{Elements: []parser.Expression{}},
			},
		}},
		"BOOLEANS = {0, 1}": {Statements: []parser.Statement{
			&parser.Assignement{
				Name: "BOOLEANS",
				Expression: &parser.EnumerationSet{Elements: []parser.Expression{
					&parser.LiteralInteger{Value: big.NewInt(0)},
					&parser.LiteralInteger{Value: big.NewInt(1)},
				}},
			},
		}},
	} {
		t.Run(testCase, func(t *testing.T) {
			t.Parallel()

			tokenizer := tokenizer.New(strings.NewReader(testCase))
			parser := parser.New(tokenizer)
			actualAST, err := parser.Parse()
			require.NoError(t, err)
			require.Equal(t, expectedAST, actualAST)
		})
	}
}
