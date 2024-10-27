package jlang_test

import (
	"strings"
	"testing"

	"github.com/juliendoutre/jlang"
	"github.com/stretchr/testify/require"
)

func TestTokenizer(t *testing.T) {
	t.Parallel()

	testCases := map[string][]*jlang.Token{
		"": {},
		"Hello": {
			{Type: jlang.IdentifierTokenType, Value: "Hello"},
		},
		"Hello World": {
			{Type: jlang.IdentifierTokenType, Value: "Hello"},
			{Type: jlang.IdentifierTokenType, Value: "World"},
		},
		"0": {
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "0"},
		},
		"1279": {
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "1279"},
		},
		"0 Hello World 1279": {
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "0"},
			{Type: jlang.IdentifierTokenType, Value: "Hello"},
			{Type: jlang.IdentifierTokenType, Value: "World"},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "1279"},
		},
		"0 > 1 & { test.value }::": {
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "0"},
			{Type: jlang.SymbolTokenType, Value: ">"},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "1"},
			{Type: jlang.SymbolTokenType, Value: "&"},
			{Type: jlang.SymbolTokenType, Value: "{"},
			{Type: jlang.IdentifierTokenType, Value: "test"},
			{Type: jlang.SymbolTokenType, Value: "."},
			{Type: jlang.IdentifierTokenType, Value: "value"},
			{Type: jlang.SymbolTokenType, Value: "}"},
			{Type: jlang.SymbolTokenType, Value: ":"},
			{Type: jlang.SymbolTokenType, Value: ":"},
		},
		"0 >= 1 & { test.value }::": {
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "0"},
			{Type: jlang.SymbolTokenType, Value: ">="},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "1"},
			{Type: jlang.SymbolTokenType, Value: "&"},
			{Type: jlang.SymbolTokenType, Value: "{"},
			{Type: jlang.IdentifierTokenType, Value: "test"},
			{Type: jlang.SymbolTokenType, Value: "."},
			{Type: jlang.IdentifierTokenType, Value: "value"},
			{Type: jlang.SymbolTokenType, Value: "}"},
			{Type: jlang.SymbolTokenType, Value: ":"},
			{Type: jlang.SymbolTokenType, Value: ":"},
		},
		"// This is a comment": {
			{Type: jlang.CommentTokenType, Value: "// This is a comment"},
		},
		"a = {0, 1, 2, 3} // This is a comment": {
			{Type: jlang.IdentifierTokenType, Value: "a"},
			{Type: jlang.SymbolTokenType, Value: "="},
			{Type: jlang.SymbolTokenType, Value: "{"},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "0"},
			{Type: jlang.SymbolTokenType, Value: ","},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "1"},
			{Type: jlang.SymbolTokenType, Value: ","},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "2"},
			{Type: jlang.SymbolTokenType, Value: ","},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "3"},
			{Type: jlang.SymbolTokenType, Value: "}"},
			{Type: jlang.CommentTokenType, Value: "// This is a comment"},
		},
		"a = {0, 1, 2, 3} // This is a comment\nb = a // Another comment\n10": {
			{Type: jlang.IdentifierTokenType, Value: "a"},
			{Type: jlang.SymbolTokenType, Value: "="},
			{Type: jlang.SymbolTokenType, Value: "{"},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "0"},
			{Type: jlang.SymbolTokenType, Value: ","},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "1"},
			{Type: jlang.SymbolTokenType, Value: ","},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "2"},
			{Type: jlang.SymbolTokenType, Value: ","},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "3"},
			{Type: jlang.SymbolTokenType, Value: "}"},
			{Type: jlang.CommentTokenType, Value: "// This is a comment"},
			{Type: jlang.IdentifierTokenType, Value: "b"},
			{Type: jlang.SymbolTokenType, Value: "="},
			{Type: jlang.IdentifierTokenType, Value: "a"},
			{Type: jlang.CommentTokenType, Value: "// Another comment"},
			{Type: jlang.LiteralDecimalIntegerTokenType, Value: "10"},
		},
	}

	for testCase, expectedTokens := range testCases {
		t.Run(testCase, func(t *testing.T) {
			t.Parallel()

			tokenizer := jlang.NewTokenizer(strings.NewReader(testCase))
			actualTokens, err := consumeAllTokens(t, tokenizer)
			require.NoError(t, err)
			require.Equal(t, expectedTokens, actualTokens)
		})
	}
}

func consumeAllTokens(t *testing.T, tokenizer *jlang.Tokenizer) ([]*jlang.Token, error) {
	t.Helper()

	tokens := []*jlang.Token{}

	for {
		token, err := tokenizer.Next()
		if err != nil {
			return nil, err
		}

		if token == nil {
			return tokens, nil
		}

		tokens = append(tokens, token)
	}
}
