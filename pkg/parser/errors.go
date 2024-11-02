package parser

import (
	"fmt"
	"strings"

	"github.com/juliendoutre/jlang/pkg/tokenizer"
)

type UnexpectedTokenError struct {
	actualType, expectedType   tokenizer.TokenType
	actualValue, expectedValue string
}

func (u *UnexpectedTokenError) Error() string {
	return fmt.Sprintf("expected a %s with value %q but read a %s with value %q", u.expectedType, u.expectedValue, u.actualType, u.actualValue)
}

type UnexpectedTokenTypesError struct {
	actual   tokenizer.TokenType
	expected []tokenizer.TokenType
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

type UndefinedVariableError struct {
	Name string
}

func (u *UndefinedVariableError) Error() string {
	return fmt.Sprintf("undefined variable %q", u.Name)
}
