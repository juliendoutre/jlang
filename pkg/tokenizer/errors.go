package tokenizer

import "fmt"

type InvalidTokenizerStateError struct {
	state TokenType
}

func (i *InvalidTokenizerStateError) Error() string {
	return fmt.Sprintf("Tokenizer reached an invalid state %s", i.state)
}

type UnexpectedRuneError struct {
	state TokenType
	r     rune
}

func (u *UnexpectedRuneError) Error() string {
	return fmt.Sprintf("read an unexpected rune %q while parsing a %s", u.r, u.state)
}

type UnclosedLiteralStringError struct{}

func (u *UnclosedLiteralStringError) Error() string {
	return "a literal string was not be closed properly"
}
