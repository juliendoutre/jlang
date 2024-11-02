package tokenizer

import "github.com/juliendoutre/jlang/pkg/sources"

type TokenType uint

const (
	UndefinedTokenType TokenType = iota
	IdentifierTokenType
	LiteralIntegerTokenType
	SymbolTokenType
	CommentTokenType
)

func (t TokenType) String() string {
	switch t {
	case UndefinedTokenType:
		return "undefined token"
	case IdentifierTokenType:
		return "identifier token"
	case LiteralIntegerTokenType:
		return "literal integer token"
	case SymbolTokenType:
		return "symbol token"
	case CommentTokenType:
		return "comment token"
	default:
		return "unknown token"
	}
}

type Token struct {
	Type         TokenType
	Value        string
	Delimitation sources.Delimitation
}
