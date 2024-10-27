package jlang

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"unicode"
)

type TokenType uint

const (
	UndefinedTokenType TokenType = iota
	IdentifierTokenType
	LiteralDecimalIntegerTokenType
	SymbolTokenType
)

func (t TokenType) String() string {
	switch t {
	case UndefinedTokenType:
		return "undefined token"
	case IdentifierTokenType:
		return "identifier token"
	case LiteralDecimalIntegerTokenType:
		return "literal decimal integer token"
	case SymbolTokenType:
		return "symbol token"
	default:
		return "unknown token"
	}
}

type Token struct {
	Type  TokenType
	Value string
}

func NewTokenizer(reader io.RuneScanner) *Tokenizer {
	return &Tokenizer{
		reader: reader,
		buffer: bytes.Buffer{},
		state:  UndefinedTokenType,
	}
}

type Tokenizer struct {
	reader io.RuneReader
	buffer bytes.Buffer
	state  TokenType
}

func (t *Tokenizer) Next() (*Token, error) {
	for {
		// Read a new rune from t.Reader.
		r, _, err := t.reader.ReadRune()
		if err != nil {
			// Handle the case where we reach the end of t.Reader.
			if errors.Is(err, io.EOF) {
				switch t.state {
				// If we were not parsing anything already, simply return a nil Token.
				case UndefinedTokenType:
					return nil, nil
				// If we were parsing something, we need to return it.
				case IdentifierTokenType, LiteralDecimalIntegerTokenType, SymbolTokenType:
					// Prepare to reset t.state and t.buffer after we returned the token.
					defer t.reset()

					return &Token{Type: t.state, Value: t.buffer.String()}, nil
				// This should never be reached.
				default:
					return nil, fmt.Errorf("Tokenizer is in an unsupported state %d", t.state)
				}
			}

			return nil, fmt.Errorf("failed reading from reader: %w", err)
		}

		// Depending on the current state of Tokenizer, the handling of the new rune may vary.
		switch t.state {
		// Tokenizer is not parsing a contiguous token (typically the first iteration of the loop).
		// t.buffer should be empty at this point.
		case UndefinedTokenType:
			// Skip any space.
			if unicode.IsSpace(r) {
				continue
			}

			// If a letter, prepare next iterations to scan an identifier.
			if isLetter(r) {
				t.state = IdentifierTokenType
				t.buffer.WriteRune(r)

				continue
			}

			// If a digit, prepare next iterations to scan a literal decimal integer.
			if unicode.IsDigit(r) {
				t.state = LiteralDecimalIntegerTokenType
				t.buffer.WriteRune(r)

				continue
			}

			// If a symbol, prepare next iterations to scan a symbol.
			if isSymbol(string(r)) {
				t.state = SymbolTokenType
				t.buffer.WriteRune(r)

				continue
			}
		// Tokenizer started parsing an identifier token in previous iterations of the loop.
		// It's content so far is stored in t.buffer.
		case IdentifierTokenType:
			// If a letter of digit, add it to the buffer and continue.
			if isLetter(r) || unicode.IsDigit(r) {
				t.buffer.WriteRune(r)

				continue
			}

			// If a space, we reached the end of the identifier.
			if unicode.IsSpace(r) {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer t.reset()

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}

			// If a symbol, we reached the end of the identifier.
			if isSymbol(string(r)) {
				// Prepare to set t.state and t.buffer to the symbol in the next iteration.
				defer t.set(SymbolTokenType, r)

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}
		// Tokenizer started parsing a literal decimal integer token in previous iterations of the loop.
		// It's content so far is stored in t.buffer.
		case LiteralDecimalIntegerTokenType:
			// If a digit, add it to the buffer and continue.
			if unicode.IsDigit(r) {
				t.buffer.WriteRune(r)

				continue
			}

			// If a space, we reached the end of the literal.
			if unicode.IsSpace(r) {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer t.reset()

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}

			// If a symbol, we reached the end of the literal.
			if isSymbol(string(r)) {
				// Prepare to set t.state and t.buffer to the symbol in the next iteration.
				defer t.set(SymbolTokenType, r)

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}
		case SymbolTokenType:
			// If a space, we reached the end of the symbol.
			if unicode.IsSpace(r) {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer t.reset()

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}

			// If a letter, we reached the end of the symbol.
			if isLetter(r) {
				// Prepare to set t.state and t.buffer to an identifier in the next iteration.
				defer t.set(IdentifierTokenType, r)

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}

			// If a digit, we reached the end of the symbol.
			if unicode.IsDigit(r) {
				// Prepare to set t.state and t.buffer to a literal decimal integer in the next iteration.
				defer t.set(LiteralDecimalIntegerTokenType, r)

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}

			// If another symbol:
			if isSymbol(string(r)) {
				// If extending the current buffer with the rune matches a valid symbol, add the rune to the buffer.
				if isSymbol(t.buffer.String() + string(r)) {
					t.buffer.WriteRune(r)

					continue
				}

				// Else we reached the end of the symbol.
				// Prepare to set t.state and t.buffer to a symbol in the next iteration.
				defer t.set(SymbolTokenType, r)

				return &Token{Type: t.state, Value: t.buffer.String()}, nil
			}
		// This should never be reached.
		default:
			return nil, fmt.Errorf("Tokenizer is in an unsupported state %d", t.state)
		}

		// In case the rune did not match any condition for the current state, it's unexpected and we return an error.
		return nil, fmt.Errorf("unexpected rune %q when parsing a %q", r, t.state)
	}
}

func (t *Tokenizer) reset() {
	t.state = UndefinedTokenType
	t.buffer.Reset()
}

func (t *Tokenizer) set(state TokenType, r rune) {
	t.state = state
	t.buffer.Reset()
	t.buffer.WriteRune(r)
}

func isLetter(r rune) bool {
	return unicode.IsLetter(r) || r == '_'
}

func isSymbol(r string) bool {
	_, ok := symbols[r]
	return ok
}

var symbols = map[string]struct{}{
	// Arithmetic
	"+":  {},
	"-":  {},
	"*":  {},
	"/":  {},
	"%":  {},
	">":  {},
	">=": {},
	"<":  {},
	"<=": {},
	// Logic
	"&": {},
	"|": {},
	"!": {},
	"^": {},
	// Assignement
	"=":  {},
	"->": {},
	// Punctuation
	".": {},
	",": {},
	":": {},
	// Grouping
	"{": {},
	"}": {},
	"(": {},
	")": {},
	"[": {},
	"]": {},
}
