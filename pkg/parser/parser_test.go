package parser_test

import (
	"math/big"
	"strings"
	"testing"

	"github.com/juliendoutre/jlang/pkg/parser"
	"github.com/juliendoutre/jlang/pkg/sources"
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
				Name: "a",
				Expression: &parser.LiteralInteger{
					Value:        big.NewInt(10),
					Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 4}, End: sources.Location{Line: 0, Column: 6}},
				},
				Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 6}},
			},
		}},
		"a = 10\nb = a": {Statements: []parser.Statement{
			&parser.Assignement{
				Name: "a",
				Expression: &parser.LiteralInteger{
					Value:        big.NewInt(10),
					Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 4}, End: sources.Location{Line: 0, Column: 6}},
				},
				Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 6}},
			},
			&parser.Assignement{
				Name: "b",
				Expression: &parser.Identifier{
					Name:         "a",
					Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 4}, End: sources.Location{Line: 1, Column: 5}},
				},
				Delimitation: sources.Delimitation{Start: sources.Location{Line: 1, Column: 0}, End: sources.Location{Line: 1, Column: 5}},
			},
		}},
		"EMPTY = {}": {Statements: []parser.Statement{
			&parser.Assignement{
				Name: "EMPTY",
				Expression: &parser.EnumerationSet{
					Elements:     []parser.Expression{},
					Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 8}, End: sources.Location{Line: 0, Column: 10}},
				},
				Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 10}},
			},
		}},
		"BOOLEANS = {0, 1}": {Statements: []parser.Statement{
			&parser.Assignement{
				Name: "BOOLEANS",
				Expression: &parser.EnumerationSet{
					Elements: []parser.Expression{
						&parser.LiteralInteger{
							Value:        big.NewInt(0),
							Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 12}, End: sources.Location{Line: 0, Column: 13}},
						},
						&parser.LiteralInteger{
							Value:        big.NewInt(1),
							Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 15}, End: sources.Location{Line: 0, Column: 16}},
						},
					},
					Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 11}, End: sources.Location{Line: 0, Column: 17}},
				},
				Delimitation: sources.Delimitation{Start: sources.Location{Line: 0, Column: 0}, End: sources.Location{Line: 0, Column: 17}},
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
