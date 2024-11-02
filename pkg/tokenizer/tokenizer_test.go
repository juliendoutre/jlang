package tokenizer_test

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/juliendoutre/jlang/pkg/sources"
	"github.com/juliendoutre/jlang/pkg/tokenizer"
	"github.com/stretchr/testify/require"
)

func TestTokenizer(t *testing.T) {
	t.Parallel()

	for testCase, expectedTokens := range map[string][]*tokenizer.Token{
		"":   {},
		"  ": {},
		"Hello": {
			{Type: tokenizer.IdentifierTokenType, Value: "Hello", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 5}}},
		},
		"Hello World": {
			{Type: tokenizer.IdentifierTokenType, Value: "Hello", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 5}}},
			{Type: tokenizer.IdentifierTokenType, Value: "World", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 6}, End: sources.Location{Line: 0, Column: 11}}},
		},
		"0": {
			{Type: tokenizer.LiteralIntegerTokenType, Value: "0", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 1}}},
		},
		"1279": {
			{Type: tokenizer.LiteralIntegerTokenType, Value: "1279", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 4}}},
		},
		"0 Hello World 1279": {
			{Type: tokenizer.LiteralIntegerTokenType, Value: "0", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 1}}},
			{Type: tokenizer.IdentifierTokenType, Value: "Hello", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 2}, End: sources.Location{Line: 0, Column: 7}}},
			{Type: tokenizer.IdentifierTokenType, Value: "World", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 8}, End: sources.Location{Line: 0, Column: 13}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "1279", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 14}, End: sources.Location{Line: 0, Column: 18}}},
		},
		"0 > 1 & { test.value }::": {
			{Type: tokenizer.LiteralIntegerTokenType, Value: "0", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 1}}},
			{Type: tokenizer.SymbolTokenType, Value: ">", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 2}, End: sources.Location{Line: 0, Column: 3}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "1", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 4}, End: sources.Location{Line: 0, Column: 5}}},
			{Type: tokenizer.SymbolTokenType, Value: "&", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 6}, End: sources.Location{Line: 0, Column: 7}}},
			{Type: tokenizer.SymbolTokenType, Value: "{", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 8}, End: sources.Location{Line: 0, Column: 9}}},
			{Type: tokenizer.IdentifierTokenType, Value: "test", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 10}, End: sources.Location{Line: 0, Column: 14}}},
			{Type: tokenizer.SymbolTokenType, Value: ".", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 14}, End: sources.Location{Line: 0, Column: 15}}},
			{Type: tokenizer.IdentifierTokenType, Value: "value", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 15}, End: sources.Location{Line: 0, Column: 20}}},
			{Type: tokenizer.SymbolTokenType, Value: "}", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 21}, End: sources.Location{Line: 0, Column: 22}}},
			{Type: tokenizer.SymbolTokenType, Value: ":", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 22}, End: sources.Location{Line: 0, Column: 23}}},
			{Type: tokenizer.SymbolTokenType, Value: ":", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 23}, End: sources.Location{Line: 0, Column: 24}}},
		},
		"0 >= 1 & { test.value }::": {
			{Type: tokenizer.LiteralIntegerTokenType, Value: "0", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 1}}},
			{Type: tokenizer.SymbolTokenType, Value: ">=", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 2}, End: sources.Location{Line: 0, Column: 4}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "1", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 5}, End: sources.Location{Line: 0, Column: 6}}},
			{Type: tokenizer.SymbolTokenType, Value: "&", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 7}, End: sources.Location{Line: 0, Column: 8}}},
			{Type: tokenizer.SymbolTokenType, Value: "{", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 9}, End: sources.Location{Line: 0, Column: 10}}},
			{Type: tokenizer.IdentifierTokenType, Value: "test", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 11}, End: sources.Location{Line: 0, Column: 15}}},
			{Type: tokenizer.SymbolTokenType, Value: ".", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 15}, End: sources.Location{Line: 0, Column: 16}}},
			{Type: tokenizer.IdentifierTokenType, Value: "value", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 16}, End: sources.Location{Line: 0, Column: 21}}},
			{Type: tokenizer.SymbolTokenType, Value: "}", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 22}, End: sources.Location{Line: 0, Column: 23}}},
			{Type: tokenizer.SymbolTokenType, Value: ":", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 23}, End: sources.Location{Line: 0, Column: 24}}},
			{Type: tokenizer.SymbolTokenType, Value: ":", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 24}, End: sources.Location{Line: 0, Column: 25}}},
		},
		"// This is a comment": {
			{Type: tokenizer.CommentTokenType, Value: "// This is a comment", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 20}}},
		},
		"a = {0, 1, 2, 43} // This is a comment": {
			{Type: tokenizer.IdentifierTokenType, Value: "a", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 1}}},
			{Type: tokenizer.SymbolTokenType, Value: "=", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 2}, End: sources.Location{Line: 0, Column: 3}}},
			{Type: tokenizer.SymbolTokenType, Value: "{", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 4}, End: sources.Location{Line: 0, Column: 5}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "0", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 5}, End: sources.Location{Line: 0, Column: 6}}},
			{Type: tokenizer.SymbolTokenType, Value: ",", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 6}, End: sources.Location{Line: 0, Column: 7}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "1", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 8}, End: sources.Location{Line: 0, Column: 9}}},
			{Type: tokenizer.SymbolTokenType, Value: ",", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 9}, End: sources.Location{Line: 0, Column: 10}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "2", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 11}, End: sources.Location{Line: 0, Column: 12}}},
			{Type: tokenizer.SymbolTokenType, Value: ",", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 12}, End: sources.Location{Line: 0, Column: 13}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "43", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 14}, End: sources.Location{Line: 0, Column: 16}}},
			{Type: tokenizer.SymbolTokenType, Value: "}", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 16}, End: sources.Location{Line: 0, Column: 17}}},
			{Type: tokenizer.CommentTokenType, Value: "// This is a comment", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 18}, End: sources.Location{Line: 0, Column: 38}}},
		},
		"a = {0, 1, 2, 43} // This is a comment\nb = a // Another comment\n10": {
			{Type: tokenizer.IdentifierTokenType, Value: "a", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 1}}},
			{Type: tokenizer.SymbolTokenType, Value: "=", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 2}, End: sources.Location{Line: 0, Column: 3}}},
			{Type: tokenizer.SymbolTokenType, Value: "{", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 4}, End: sources.Location{Line: 0, Column: 5}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "0", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 5}, End: sources.Location{Line: 0, Column: 6}}},
			{Type: tokenizer.SymbolTokenType, Value: ",", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 6}, End: sources.Location{Line: 0, Column: 7}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "1", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 8}, End: sources.Location{Line: 0, Column: 9}}},
			{Type: tokenizer.SymbolTokenType, Value: ",", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 9}, End: sources.Location{Line: 0, Column: 10}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "2", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 11}, End: sources.Location{Line: 0, Column: 12}}},
			{Type: tokenizer.SymbolTokenType, Value: ",", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 12}, End: sources.Location{Line: 0, Column: 13}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "43", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 14}, End: sources.Location{Line: 0, Column: 16}}},
			{Type: tokenizer.SymbolTokenType, Value: "}", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 16}, End: sources.Location{Line: 0, Column: 17}}},
			{Type: tokenizer.CommentTokenType, Value: "// This is a comment", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 18}, End: sources.Location{Line: 0, Column: 38}}},
			{Type: tokenizer.IdentifierTokenType, Value: "b", Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 0}, End: sources.Location{Line: 1, Column: 1}}},
			{Type: tokenizer.SymbolTokenType, Value: "=", Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 2}, End: sources.Location{Line: 1, Column: 3}}},
			{Type: tokenizer.IdentifierTokenType, Value: "a", Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 4}, End: sources.Location{Line: 1, Column: 5}}},
			{Type: tokenizer.CommentTokenType, Value: "// Another comment", Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 6}, End: sources.Location{Line: 1, Column: 24}}},
			{Type: tokenizer.LiteralIntegerTokenType, Value: "10", Delimitation: sources.Delimitation{Start: sources.Location{Line: 2, Column: 0}, End: sources.Location{Line: 2, Column: 2}}},
		},
		"a = \"this is a string!\"\nb = \"another string\"": {
			{Type: tokenizer.IdentifierTokenType, Value: "a", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 1}}},
			{Type: tokenizer.SymbolTokenType, Value: "=", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 2}, End: sources.Location{Line: 0, Column: 3}}},
			{Type: tokenizer.LiteralStringTokenType, Value: "\"this is a string!\"", Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 4}, End: sources.Location{Line: 0, Column: 23}}},
			{Type: tokenizer.IdentifierTokenType, Value: "b", Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 0}, End: sources.Location{Line: 1, Column: 1}}},
			{Type: tokenizer.SymbolTokenType, Value: "=", Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 2}, End: sources.Location{Line: 1, Column: 3}}},
			{Type: tokenizer.LiteralStringTokenType, Value: "\"another string\"", Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 4}, End: sources.Location{Line: 1, Column: 20}}},
		},
	} {
		t.Run(testCase, func(t *testing.T) {
			t.Parallel()

			tokenizer := tokenizer.New(strings.NewReader(testCase))
			actualTokens, err := consumeAllTokens(t, tokenizer)
			require.NoError(t, err)
			require.Equal(t, expectedTokens, actualTokens)
		})
	}
}

func consumeAllTokens(t *testing.T, tok *tokenizer.Tokenizer) ([]*tokenizer.Token, error) {
	t.Helper()

	tokens := []*tokenizer.Token{}

	for {
		token, err := tok.Next()
		if err != nil {
			if errors.Is(err, io.EOF) {
				return tokens, nil
			}

			return nil, fmt.Errorf("failed getting next token: %w", err)
		}

		tokens = append(tokens, token)
	}
}
