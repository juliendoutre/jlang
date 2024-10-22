package jlang

import "io"

type Tokenizer struct {
	Reader io.RuneReader
}

func (t *Tokenizer) NextToken() (*Token, error) {
	return &Token{}, nil
}

type Token struct {
	Start, End Location
	Value      string
}

type Location struct {
	Line, Column uint
}
