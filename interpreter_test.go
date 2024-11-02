package jlang_test

import (
	"strings"
	"testing"

	"github.com/juliendoutre/jlang"
	"github.com/stretchr/testify/require"
)

func TestInterpreter(t *testing.T) {
	t.Parallel()

	for testCase, expectedError := range map[string]error{
		"":              nil,
		"   ":           nil,
		"a = 10":        nil,
		"a = 10\nb = a": nil,
		"b = a":         &jlang.DelimitedError{Err: &jlang.UndefinedVariableError{Name: "a"}, Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 4}, End: jlang.Location{Line: 0, Column: 5}}},
	} {
		t.Run(testCase, func(t *testing.T) {
			t.Parallel()

			tokenizer := jlang.NewTokenizer(strings.NewReader(testCase))
			parser := jlang.NewParser(tokenizer)
			ast, err := parser.Parse()
			require.NoError(t, err)

			interpreter := jlang.NewInterpreter()
			actualErr := interpreter.Run(ast)
			require.Equal(t, expectedError, actualErr)
		})
	}
}
