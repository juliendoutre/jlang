package interpreter_test

import (
	"strings"
	"testing"

	"github.com/juliendoutre/jlang/pkg/interpreter"
	"github.com/juliendoutre/jlang/pkg/parser"
	"github.com/juliendoutre/jlang/pkg/sources"
	"github.com/juliendoutre/jlang/pkg/tokenizer"
	"github.com/stretchr/testify/require"
)

func TestInterpreter(t *testing.T) {
	t.Parallel()

	for testCase, expectedError := range map[string]error{
		"":              nil,
		"   ":           nil,
		"a = 10":        nil,
		"a = 10\nb = a": nil,
		"b = a":         &sources.DelimitedError{Err: &parser.UndefinedVariableError{Name: "a"}, Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 4}, End: sources.Location{Line: 0, Column: 5}}},
	} {
		t.Run(testCase, func(t *testing.T) {
			t.Parallel()

			tokenizer := tokenizer.New(strings.NewReader(testCase))
			parser := parser.New(tokenizer)
			ast, err := parser.Parse()
			require.NoError(t, err)

			interpreter := interpreter.New()
			actualErr := interpreter.Run(ast)
			require.Equal(t, expectedError, actualErr)
		})
	}
}
