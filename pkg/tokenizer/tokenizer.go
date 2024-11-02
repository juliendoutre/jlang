package tokenizer

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"unicode"
	"unicode/utf8"

	"github.com/juliendoutre/jlang/pkg/sources"
)

func New(reader io.RuneReader) *Tokenizer {
	return &Tokenizer{
		reader: reader,
		buffer: bytes.Buffer{},
		state:  UndefinedTokenType,
		cursor: sources.Location{
			Column: 0,
			Line:   0,
		},
	}
}

type Tokenizer struct {
	reader io.RuneReader
	buffer bytes.Buffer
	state  TokenType
	cursor sources.Location
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
					return nil, io.EOF
				// If we were parsing something, we need to return it.
				case IdentifierTokenType, LiteralIntegerTokenType, SymbolTokenType, CommentTokenType:
					// Prepare to reset t.state and t.buffer after we returned the token.
					defer t.reset()
					defer t.increment(r)

					return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
				// This should never be reached.
				default:
					return nil, &sources.LocatedError{Err: &InvalidTokenizerStateError{state: t.state}, Location: t.cursor}
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
				t.increment(r)

				continue
			}

			// If a letter, prepare next iterations to scan an identifier.
			if isLetter(r) {
				t.state = IdentifierTokenType
				t.buffer.WriteRune(r)
				t.increment(r)

				continue
			}

			// If a digit, prepare next iterations to scan a literal integer.
			if unicode.IsDigit(r) {
				t.state = LiteralIntegerTokenType
				t.buffer.WriteRune(r)
				t.increment(r)

				continue
			}

			// If a symbol, prepare next iterations to scan a symbol.
			if isSymbol(string(r)) {
				t.state = SymbolTokenType
				t.buffer.WriteRune(r)
				t.increment(r)

				continue
			}
		// Tokenizer started parsing an identifier token in previous iterations of the loop.
		// It's content so far is stored in t.buffer.
		case IdentifierTokenType:
			// If a letter of digit, add it to the buffer and continue.
			if isLetter(r) || unicode.IsDigit(r) {
				t.buffer.WriteRune(r)
				t.increment(r)

				continue
			}

			// If a space, we reached the end of the identifier.
			if unicode.IsSpace(r) {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer t.reset()
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}

			// If a symbol, we reached the end of the identifier.
			if isSymbol(string(r)) {
				// Prepare to set t.state and t.buffer to the symbol in the next iteration.
				defer t.set(SymbolTokenType, r)
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}
		// Tokenizer started parsing a literal integer token in previous iterations of the loop.
		// It's content so far is stored in t.buffer.
		case LiteralIntegerTokenType:
			// If a digit, add it to the buffer and continue.
			if unicode.IsDigit(r) {
				t.buffer.WriteRune(r)
				t.increment(r)

				continue
			}

			// If a space, we reached the end of the literal.
			if unicode.IsSpace(r) {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer t.reset()
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}

			// If a symbol, we reached the end of the literal.
			if isSymbol(string(r)) {
				// Prepare to set t.state and t.buffer to the symbol in the next iteration.
				defer t.set(SymbolTokenType, r)
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}
		// Tokenizer started parsing a symbol in previous iterations of the loop.
		// It's content so far is stored in t.buffer.
		case SymbolTokenType:
			// If a space, we reached the end of the symbol.
			if unicode.IsSpace(r) {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer t.reset()
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}

			// If a letter, we reached the end of the symbol.
			if isLetter(r) {
				// Prepare to set t.state and t.buffer to an identifier in the next iteration.
				defer t.set(IdentifierTokenType, r)
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}

			// If a digit, we reached the end of the symbol.
			if unicode.IsDigit(r) {
				// Prepare to set t.state and t.buffer to a literal integer in the next iteration.
				defer t.set(LiteralIntegerTokenType, r)
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}

			// If another symbol:
			if isSymbol(string(r)) {
				// If extending the current buffer with the rune matches a valid symbol, add the rune to the buffer.
				if isSymbol(t.buffer.String() + string(r)) {
					t.buffer.WriteRune(r)
					t.increment(r)

					// If the current symbol indicates the start of a comment, we set the state accordingly for next iterations.
					if t.buffer.String() == "//" {
						t.state = CommentTokenType
					}

					continue
				}

				// Else we reached the end of the symbol.
				// Prepare to set t.state and t.buffer to a symbol in the next iteration.
				defer t.set(SymbolTokenType, r)
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}
		// Tokenizer started parsing a comment in previous iterations of the loop.
		// It's content so far is stored in t.buffer.
		case CommentTokenType:
			// Comments end when a new line is started.
			if r == '\n' {
				// Prepare to reset t.state and t.buffer after we returned the token.
				defer t.reset()
				defer t.increment(r)

				return &Token{Type: t.state, Value: t.buffer.String(), Delimitation: sources.Delimitation{Start: t.getStart(), End: t.cursor}}, nil
			}

			// Any other printable character is considered part of the comment.
			if unicode.IsPrint(r) {
				t.buffer.WriteRune(r)
				t.increment(r)

				continue
			}
		// This should never be reached.
		default:
			return nil, &sources.LocatedError{Err: &InvalidTokenizerStateError{state: t.state}, Location: t.cursor}
		}

		// In case the rune did not match any condition for the current state, it's unexpected and we return an error.
		return nil, &sources.LocatedError{Err: &UnexpectedRuneError{state: t.state, r: r}, Location: t.cursor}
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

func (t *Tokenizer) increment(r rune) {
	if r == '\n' {
		t.cursor.Line++
		t.cursor.Column = 0
	} else {
		t.cursor.Column++
	}
}

func (t *Tokenizer) getStart() sources.Location {
	return sources.Location{
		Line:   t.cursor.Line,
		Column: t.cursor.Column - uint(utf8.RuneCount(t.buffer.Bytes())),
	}
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
	".":   {},
	",":   {},
	":":   {},
	"...": {},
	// Grouping
	"{": {},
	"}": {},
	"(": {},
	")": {},
	"[": {},
	"]": {},
	// Comments
	"//": {},
}
