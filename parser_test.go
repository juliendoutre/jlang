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
			jlang.Assignement{
				Name:       "a",
				Expression: jlang.LiteralInteger{Value: big.NewInt(10)},
			},
		}},
		"a = 10\nb = a": {Statements: []jlang.Statement{
			jlang.Assignement{
				Name:       "a",
				Expression: jlang.LiteralInteger{Value: big.NewInt(10)},
			},
			jlang.Assignement{
				Name:       "b",
				Expression: jlang.Identifier{Name: "a"},
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