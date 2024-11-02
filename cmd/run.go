package main

import (
	"fmt"

	"github.com/juliendoutre/jlang/pkg/interpreter"
	"github.com/juliendoutre/jlang/pkg/parser"
	"github.com/juliendoutre/jlang/pkg/sources"
	"github.com/juliendoutre/jlang/pkg/tokenizer"
	"github.com/spf13/cobra"
)

func runCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "run",
		Short: "Run a JLang source code file.",
		RunE: func(_ *cobra.Command, args []string) error {
			if len(args) < 1 {
				return fmt.Errorf("Expected a path to a source file")
			}

			if len(args) < 2 {
				return fmt.Errorf("Expected a digest for the source file")
			}

			module, err := sources.New(args[0], args[1])
			if err != nil {
				return fmt.Errorf("failed to load a module: %w", err)
			}

			reader, err := module.Load()
			if err != nil {
				return fmt.Errorf("failed to load a module: %w", err)
			}

			parser := parser.New(tokenizer.New(reader))
			ast, err := parser.Parse()
			if err != nil {
				return fmt.Errorf("failed to parse a program: %w", err)
			}

			interpreter := interpreter.New()
			if err := interpreter.Run(ast); err != nil {
				return fmt.Errorf("failed to run a program: %w", err)
			}

			return nil
		},
	}

	return cmd
}
