package jlang

type Lexer struct{}

func (l *Lexer) Lex(tokenizer *Tokenizer) (*SyntaxTree, error) {
	return &SyntaxTree{}, nil
}

type SyntaxTree struct{}
