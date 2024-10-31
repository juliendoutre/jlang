package jlang

import (
	"math/big"
)

// AST stands for Abstract Syntax Tree.
// It is a structured representation of the code.
type AST struct {
	// Any program is a list of statements.
	Statements []Statement
}

// A Statement does not return a value.
type Statement interface{}

// A VariableAssignement associates an Expression with a Name that can be referenced later.
type VariableAssignement struct {
	Name       string
	Expression Expression
}

// An expression evaluates to a Value in the context of an Execution.
type Expression interface {
	Evaluate(context Execution) (Value, error)
}

// A Value can represent many things but is always simply just a bunch of bytes.
type Value interface {
	Bytes() []byte
}

// A LiteralInteger represents an integer Value.
type LiteralInteger struct {
	Value *big.Int
}

// It is both an Expression...
func (l *LiteralInteger) Evaluate(_ Execution) (Value, error) {
	return l, nil
}

// ... and a Value!
func (l *LiteralInteger) Bytes() []byte {
	return l.Value.Bytes()
}

// An Identifier refers to a previously assigned Expression.
type Identifier struct {
	Name string
}

func (i *Identifier) Evaluate(context Execution) (Value, error) {
	value, ok := context.Variables[i.Name]

	if !ok {
		return nil, &UndefinedVariableError{Name: i.Name}
	}

	return value, nil
}

// A SetAssignement associates a Set with a Name that can be referenced later.
type SetAssignement struct {
	Name string
	Set  Set
}

// A Set defines a set of items.
type Set interface {
	// The number of items in a Set is called its Cardinality.
	Cardinality() uint64
}

// EnumerationSet defines a Set by listing all its elements.
type EnumerationSet struct {
	Elements []Expression
}

func (e *EnumerationSet) Cardinality() uint64 {
	return uint64(len(e.Elements))
}
