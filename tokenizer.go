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
	UnknownTokenType TokenType = iota
	IdentifierTokenType
)

type Token struct {
	Type  TokenType
	Value string
}

func NewTokenizer(reader io.RuneScanner) *Tokenizer {
	return &Tokenizer{
		reader: reader,
		buffer: bytes.Buffer{},
		state:  UnknownTokenType,
	}
}

type Tokenizer struct {
	reader io.RuneScanner
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
				case UnknownTokenType:
					return nil, nil
				// If we were parsing an identifier, we need to return it.
				case IdentifierTokenType:
					// Prepare to reset t.state and t.buffer after we returned the token.
					defer func() {
						t.state = UnknownTokenType
						t.buffer.Reset()
					}()

					return &Token{Type: IdentifierTokenType, Value: t.buffer.String()}, nil
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
		case UnknownTokenType:
			// Skip any space.
			if unicode.IsSpace(r) {
				continue
			}

			// If a letter, prepare next iterations to scan an identifier.
			if unicode.IsLetter(r) || r == '_' {
				t.state = IdentifierTokenType
				t.buffer.WriteRune(r)

				continue
			}

		// Tokenizer started parsing an identifier token in previous iterations of the loop.
		// It's content so far is stored in t.buffer.
		case IdentifierTokenType:
			// If a letter of digit, add it to the buffer and continue.
			if unicode.IsLetter(r) || r == '_' || unicode.IsDigit(r) {
				t.buffer.WriteRune(r)

				continue
			}

			// If a space, we reached the end of the identifier.
			if unicode.IsSpace(r) {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer func() {
					t.state = UnknownTokenType
					t.buffer.Reset()
				}()

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
