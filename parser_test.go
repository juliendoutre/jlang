package jlang_test

import (
	"math/big"
	"strings"
	"testing"

	"github.com/juliendoutre/jlang"
	"github.com/stretchr/testify/require"
)

func TestParser(t *testing.T) {
	t.Parallel()

	for testCase, expectedAST := range map[string]*jlang.AST{
		"":    {Statements: []jlang.Statement{}},
		"   ": {Statements: []jlang.Statement{}},
		"a = 10": {Statements: []jlang.Statement{
			&jlang.VariableAssignement{
				Name:       "a",
				Expression: &jlang.LiteralInteger{Value: big.NewInt(10)},
			},
		}},
		"a = 10\nb = a": {Statements: []jlang.Statement{
			&jlang.VariableAssignement{
				Name:       "a",
				Expression: &jlang.LiteralInteger{Value: big.NewInt(10)},
			},
			&jlang.VariableAssignement{
				Name:       "b",
				Expression: &jlang.Identifier{Name: "a"},
			},
		}},
		"b = a": {Statements: []jlang.Statement{
			&jlang.VariableAssignement{
				Name:       "b",
				Expression: &jlang.Identifier{Name: "a"},
			},
		}},
		"EMPTY = {}": {Statements: []jlang.Statement{
			&jlang.SetAssignement{
				Name: "EMPTY",
				Set:  &jlang.EnumerationSet{Elements: []jlang.Expression{}},
			},
		}},
		"BOOLEANS = {0, 1}": {Statements: []jlang.Statement{
			&jlang.SetAssignement{
				Name: "BOOLEANS",
				Set: &jlang.EnumerationSet{Elements: []jlang.Expression{
					&jlang.LiteralInteger{Value: big.NewInt(0)},
					&jlang.LiteralInteger{Value: big.NewInt(1)},
				}},
			},
		}},
	} {
		t.Run(testCase, func(t *testing.T) {
			t.Parallel()

			tokenizer := jlang.NewTokenizer(strings.NewReader(testCase))
			parser := jlang.NewParser(tokenizer)
			actualAST, err := parser.Parse()
			require.NoError(t, err)
			require.Equal(t, expectedAST, actualAST)
		})
	}
}
