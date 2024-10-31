package jlang

import (
	"fmt"
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

type Interpreter struct{}

func (i *Interpreter) Run(ast *AST) error {
	execution := NewExecution()

	for _, statement := range ast.Statements {
		if variableAssignement, ok := statement.(*Assignement); ok {
			if set, ok := variableAssignement.Expression.(Set); ok {
				execution.Sets[variableAssignement.Name] = set
			} else {
				value, err := variableAssignement.Expression.Evaluate(execution)
				if err != nil {
					return err
				}

				execution.Variables[variableAssignement.Name] = value
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
