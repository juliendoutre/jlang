package jlang

import (
	"fmt"
	"math/big"
)

func NewExecution() Execution {
	return Execution{
		Sets:      map[string]Set{},
		Variables: map[string]Value{},
	}
}

type Execution struct {
	Sets      map[string]Set
	Variables map[string]Value
}

func NewInterpreter() *Interpreter {
	return &Interpreter{}
}

type Interpreter struct {
	memory map[string]*big.Int
}

func (i *Interpreter) Run(ast *AST) error {
	execution := NewExecution()

	for _, statement := range ast.Statements {
		if variableAssignement, ok := statement.(*VariableAssignement); ok {
			value, err := variableAssignement.Expression.Evaluate(execution)
			if err != nil {
				return err
			}

			execution.Variables[variableAssignement.Name] = value
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
