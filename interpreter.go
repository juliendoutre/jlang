package jlang

import (
	"fmt"
	"math/big"
)

func NewInterpreter() *Interpreter {
	return &Interpreter{memory: map[string]*big.Int{}}
}

type Interpreter struct {
	memory map[string]*big.Int
}

func (i *Interpreter) Run(ast *AST) error {
	for _, statement := range ast.Statements {
		if assignement, ok := statement.(Assignement); ok {
			if literal, ok := assignement.Expression.(LiteralInteger); ok {
				i.memory[assignement.Name] = literal.Value
			}

			if identifier, ok := assignement.Expression.(Identifier); ok {
				if value, ok := i.memory[identifier.Name]; ok {
					i.memory[assignement.Name] = value
				} else {
					return &UndefinedVariableError{Name: identifier.Name}
				}
			}
		}
	}

	return nil
}

type UndefinedVariableError struct {
	Name string
}

func (u *UndefinedVariableError) Error() string {
	return fmt.Sprintf("undefined variable %q", u.Name)
}
