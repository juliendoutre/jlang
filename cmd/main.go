package main

import (
	"os"

	"github.com/spf13/cobra"
)

func main() {
	cmd := &cobra.Command{
		Use:          "jlang",
		Short:        "JLang toolchain",
		SilenceUsage: true,
	}

	cmd.AddCommand(
		versionCmd(),
		runCmd(),
	)

	if err := cmd.Execute(); err != nil {
		os.Exit(1)
	}
}
