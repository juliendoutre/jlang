package jlang_test

import (
	"strings"
	"testing"

	"github.com/juliendoutre/jlang"
	"github.com/stretchr/testify/require"
)

func TestTokenizer(t *testing.T) {
	t.Parallel()

	testCases := []string{
		"Hello World",
		"Hello World this is a more complicated sentence",
	}

	for _, testCase := range testCases {
		t.Run(testCase, func(t *testing.T) {
			t.Parallel()

			tokenizer := jlang.NewTokenizer(strings.NewReader(testCase))
			tokens, err := consumeAllTokens(t, tokenizer)
			require.NoError(t, err)

			actualTokens := []string{}

			for _, token := range tokens {
				require.Equal(t, jlang.IdentifierTokenType, token.Type)
				actualTokens = append(actualTokens, token.Value)
			}

			expectedTokens := strings.Fields(testCase)
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
