package interpreter

import (
	"github.com/juliendoutre/jlang/pkg/parser"
	"github.com/juliendoutre/jlang/pkg/sources"
)

func New() *Interpreter {
	return &Interpreter{}
}

type Interpreter struct{}

func (i *Interpreter) Run(ast *parser.AST) error {
	context := parser.NewContext()

	for _, statement := range ast.Statements {
		if variableAssignement, ok := statement.(*parser.Assignement); ok {
			if set, ok := variableAssignement.Expression.(parser.Set); ok {
				context.Sets[variableAssignement.Name] = set
			} else {
				value, err := variableAssignement.Expression.Evaluate(context)
				if err != nil {
					return &sources.DelimitedError{Err: err, Delimitation: variableAssignement.Expression.Delimit()}
				}

				context.Variables[variableAssignement.Name] = value
			}
		}
	}

	return nil
}
