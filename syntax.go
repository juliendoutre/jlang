package jlang

import "math/big"

type AST struct {
	Statements []Statement
}

type Statement interface{}

type Assignement struct {
	Name       string
	Expression Expression
}

type Expression interface{}

type LiteralInteger struct {
	Value *big.Int
}

type Identifier struct {
	Name string
}
