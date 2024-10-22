package jlang

type Optimizer struct{}

func (o *Optimizer) Optimize(tree *AbstractSyntaxTree) (*AbstractSyntaxTree, error) {
	return tree, nil
}
