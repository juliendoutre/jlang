package main

import (
	"bufio"
	"flag"
	"os"

	"github.com/juliendoutre/jlang"
)

var path string

func main() {
	flag.StringVar(&path, "path", "", "Path to a Jlang file.")
	flag.Parse()

	sourceFile, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	defer sourceFile.Close()

	tokenizer := &jlang.Tokenizer{Reader: bufio.NewReader(sourceFile)}

	lexer := &jlang.Lexer{}

	syntaxTree, err := lexer.Lex(tokenizer)
	if err != nil {
		panic(err)
	}

	parser := &jlang.Parser{}

	tree, err := parser.Parse(syntaxTree)
	if err != nil {
		panic(err)
	}

	optimizer := &jlang.Optimizer{}

	tree, err = optimizer.Optimize(tree)
	if err != nil {
		panic(err)
	}

	interpreter := &jlang.Interpreter{}

	if err := interpreter.Interpret(tree); err != nil {
		panic(err)
	}
}
