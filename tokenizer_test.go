package jlang_test

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/juliendoutre/jlang"
	"github.com/stretchr/testify/require"
)

func TestTokenizer(t *testing.T) {
	t.Parallel()

	for testCase, expectedTokens := range map[string][]*jlang.Token{
		"":   {},
		"  ": {},
		"Hello": {
			{Type: jlang.IdentifierTokenType, Value: "Hello", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 5}}},
		},
		"Hello World": {
			{Type: jlang.IdentifierTokenType, Value: "Hello", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 5}}},
			{Type: jlang.IdentifierTokenType, Value: "World", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 6}, End: jlang.Location{Line: 0, Column: 11}}},
		},
		"0": {
			{Type: jlang.LiteralIntegerTokenType, Value: "0", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 1}}},
		},
		"1279": {
			{Type: jlang.LiteralIntegerTokenType, Value: "1279", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 4}}},
		},
		"0 Hello World 1279": {
			{Type: jlang.LiteralIntegerTokenType, Value: "0", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 1}}},
			{Type: jlang.IdentifierTokenType, Value: "Hello", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 2}, End: jlang.Location{Line: 0, Column: 7}}},
			{Type: jlang.IdentifierTokenType, Value: "World", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 8}, End: jlang.Location{Line: 0, Column: 13}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "1279", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 14}, End: jlang.Location{Line: 0, Column: 18}}},
		},
		"0 > 1 & { test.value }::": {
			{Type: jlang.LiteralIntegerTokenType, Value: "0", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 1}}},
			{Type: jlang.SymbolTokenType, Value: ">", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 2}, End: jlang.Location{Line: 0, Column: 3}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "1", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 4}, End: jlang.Location{Line: 0, Column: 5}}},
			{Type: jlang.SymbolTokenType, Value: "&", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 6}, End: jlang.Location{Line: 0, Column: 7}}},
			{Type: jlang.SymbolTokenType, Value: "{", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 8}, End: jlang.Location{Line: 0, Column: 9}}},
			{Type: jlang.IdentifierTokenType, Value: "test", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 10}, End: jlang.Location{Line: 0, Column: 14}}},
			{Type: jlang.SymbolTokenType, Value: ".", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 14}, End: jlang.Location{Line: 0, Column: 15}}},
			{Type: jlang.IdentifierTokenType, Value: "value", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 15}, End: jlang.Location{Line: 0, Column: 20}}},
			{Type: jlang.SymbolTokenType, Value: "}", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 21}, End: jlang.Location{Line: 0, Column: 22}}},
			{Type: jlang.SymbolTokenType, Value: ":", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 22}, End: jlang.Location{Line: 0, Column: 23}}},
			{Type: jlang.SymbolTokenType, Value: ":", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 23}, End: jlang.Location{Line: 0, Column: 24}}},
		},
		"0 >= 1 & { test.value }::": {
			{Type: jlang.LiteralIntegerTokenType, Value: "0", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 1}}},
			{Type: jlang.SymbolTokenType, Value: ">=", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 2}, End: jlang.Location{Line: 0, Column: 4}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "1", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 5}, End: jlang.Location{Line: 0, Column: 6}}},
			{Type: jlang.SymbolTokenType, Value: "&", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 7}, End: jlang.Location{Line: 0, Column: 8}}},
			{Type: jlang.SymbolTokenType, Value: "{", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 9}, End: jlang.Location{Line: 0, Column: 10}}},
			{Type: jlang.IdentifierTokenType, Value: "test", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 11}, End: jlang.Location{Line: 0, Column: 15}}},
			{Type: jlang.SymbolTokenType, Value: ".", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 15}, End: jlang.Location{Line: 0, Column: 16}}},
			{Type: jlang.IdentifierTokenType, Value: "value", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 16}, End: jlang.Location{Line: 0, Column: 21}}},
			{Type: jlang.SymbolTokenType, Value: "}", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 22}, End: jlang.Location{Line: 0, Column: 23}}},
			{Type: jlang.SymbolTokenType, Value: ":", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 23}, End: jlang.Location{Line: 0, Column: 24}}},
			{Type: jlang.SymbolTokenType, Value: ":", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 24}, End: jlang.Location{Line: 0, Column: 25}}},
		},
		"// This is a comment": {
			{Type: jlang.CommentTokenType, Value: "// This is a comment", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 20}}},
		},
		"a = {0, 1, 2, 43} // This is a comment": {
			{Type: jlang.IdentifierTokenType, Value: "a", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 1}}},
			{Type: jlang.SymbolTokenType, Value: "=", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 2}, End: jlang.Location{Line: 0, Column: 3}}},
			{Type: jlang.SymbolTokenType, Value: "{", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 4}, End: jlang.Location{Line: 0, Column: 5}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "0", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 5}, End: jlang.Location{Line: 0, Column: 6}}},
			{Type: jlang.SymbolTokenType, Value: ",", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 6}, End: jlang.Location{Line: 0, Column: 7}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "1", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 8}, End: jlang.Location{Line: 0, Column: 9}}},
			{Type: jlang.SymbolTokenType, Value: ",", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 9}, End: jlang.Location{Line: 0, Column: 10}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "2", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 11}, End: jlang.Location{Line: 0, Column: 12}}},
			{Type: jlang.SymbolTokenType, Value: ",", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 12}, End: jlang.Location{Line: 0, Column: 13}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "43", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 14}, End: jlang.Location{Line: 0, Column: 16}}},
			{Type: jlang.SymbolTokenType, Value: "}", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 16}, End: jlang.Location{Line: 0, Column: 17}}},
			{Type: jlang.CommentTokenType, Value: "// This is a comment", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 18}, End: jlang.Location{Line: 0, Column: 38}}},
		},
		"a = {0, 1, 2, 43} // This is a comment\nb = a // Another comment\n10": {
			{Type: jlang.IdentifierTokenType, Value: "a", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 0}, End: jlang.Location{Line: 0, Column: 1}}},
			{Type: jlang.SymbolTokenType, Value: "=", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 2}, End: jlang.Location{Line: 0, Column: 3}}},
			{Type: jlang.SymbolTokenType, Value: "{", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 4}, End: jlang.Location{Line: 0, Column: 5}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "0", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 5}, End: jlang.Location{Line: 0, Column: 6}}},
			{Type: jlang.SymbolTokenType, Value: ",", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 6}, End: jlang.Location{Line: 0, Column: 7}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "1", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 8}, End: jlang.Location{Line: 0, Column: 9}}},
			{Type: jlang.SymbolTokenType, Value: ",", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 9}, End: jlang.Location{Line: 0, Column: 10}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "2", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 11}, End: jlang.Location{Line: 0, Column: 12}}},
			{Type: jlang.SymbolTokenType, Value: ",", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 12}, End: jlang.Location{Line: 0, Column: 13}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "43", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 14}, End: jlang.Location{Line: 0, Column: 16}}},
			{Type: jlang.SymbolTokenType, Value: "}", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 16}, End: jlang.Location{Line: 0, Column: 17}}},
			{Type: jlang.CommentTokenType, Value: "// This is a comment", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 0, Column: 18}, End: jlang.Location{Line: 0, Column: 38}}},
			{Type: jlang.IdentifierTokenType, Value: "b", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 1, Column: 0}, End: jlang.Location{Line: 1, Column: 1}}},
			{Type: jlang.SymbolTokenType, Value: "=", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 1, Column: 2}, End: jlang.Location{Line: 1, Column: 3}}},
			{Type: jlang.IdentifierTokenType, Value: "a", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 1, Column: 4}, End: jlang.Location{Line: 1, Column: 5}}},
			{Type: jlang.CommentTokenType, Value: "// Another comment", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 1, Column: 6}, End: jlang.Location{Line: 1, Column: 24}}},
			{Type: jlang.LiteralIntegerTokenType, Value: "10", Delimitation: jlang.Delimitation{Start: jlang.Location{Line: 2, Column: 0}, End: jlang.Location{Line: 2, Column: 2}}},
		},
	} {
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
			if errors.Is(err, io.EOF) {
				return tokens, nil
			}

			return nil, fmt.Errorf("failed getting next token: %w", err)
		}

		tokens = append(tokens, token)
	}
}
