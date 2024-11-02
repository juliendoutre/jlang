package parser

import (
	"math/big"

	"github.com/juliendoutre/jlang/pkg/sources"
)

// AST stands for Abstract Syntax Tree.
// It is a structured representation of the code.
type AST struct {
	// Any program is a list of statements.
	Statements []Statement
}

// A Statement does not return a value.
type Statement interface {
	sources.Delimitable
}

// An Assignement associates an Expression with a Name that can be referenced later.
type Assignement struct {
	Name         string
	Expression   Expression
	Delimitation sources.Delimitation
}

func (a *Assignement) Delimit() sources.Delimitation {
	return a.Delimitation
}

// An expression evaluates to a Value in the context of an Execution.
type Expression interface {
	Evaluate(context Context) (Value, error)

	sources.Delimitable
}

// A Value can represent many things but is always simply just a bunch of bytes.
type Value interface {
	Bytes() []byte
}

// A LiteralInteger represents an integer Value.
type LiteralInteger struct {
	Value        *big.Int
	Delimitation sources.Delimitation
}

func (l *LiteralInteger) Delimit() sources.Delimitation {
	return l.Delimitation
}

// It is both an Expression...
func (l *LiteralInteger) Evaluate(_ Context) (Value, error) {
	return l, nil
}

// ... and a Value!
func (l *LiteralInteger) Bytes() []byte {
	return l.Value.Bytes()
}

// An Identifier refers to a previously assigned Expression.
type Identifier struct {
	Name         string
	Delimitation sources.Delimitation
}

func (i *Identifier) Delimit() sources.Delimitation {
	return i.Delimitation
}

func (i *Identifier) Evaluate(context Context) (Value, error) {
	value, ok := context.Variables[i.Name]

	if !ok {
		return nil, &UndefinedVariableError{Name: i.Name}
	}

	return value, nil
}

// A Set defines a set of items.
type Set interface {
	// A Set is an abstract expression.
	Expression

	// The number of items in a Set is called its Cardinality.
	Cardinality() uint64
}

// EnumerationSet defines a Set by listing all its elements.
type EnumerationSet struct {
	Elements     []Expression
	Delimitation sources.Delimitation
}

func (e *EnumerationSet) Delimit() sources.Delimitation {
	return e.Delimitation
}

func (e *EnumerationSet) Evaluate(_ Context) (Value, error) {
	return nil, nil
}

func (e *EnumerationSet) Cardinality() uint64 {
	return uint64(len(e.Elements))
}
