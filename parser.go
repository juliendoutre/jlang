package jlang

type Parser struct{}

func (p *Parser) Parse(tree *SyntaxTree) (*AbstractSyntaxTree, error) {
	return &AbstractSyntaxTree{}, nil
}

type AbstractSyntaxTree struct{}
