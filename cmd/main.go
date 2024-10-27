package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"path"

	"github.com/juliendoutre/jlang"
)

var sourcePath string

func main() {
	flag.StringVar(&sourcePath, "path", "", "Path to a Jlang file.")
	flag.Parse()

	if path.Ext(sourcePath) != jlang.FileExtension {
		panic(fmt.Errorf("invalid file extension %q", path.Ext(sourcePath)))
	}

	sourceFile, err := os.Open(sourcePath)
	if err != nil {
		panic(err)
	}
	defer sourceFile.Close()

	tokenizer := jlang.NewTokenizer(bufio.NewReader(sourceFile))

	for {
		token, err := tokenizer.Next()
		if err != nil {
			panic(err)
		}

		if token == nil {
			break
		}

		fmt.Println(token)
	}
}
