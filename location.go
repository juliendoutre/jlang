package jlang

// A Location identifies a specific rune in a UTF-8 encoded text.
type Location struct {
	Line, Column uint
}

// A LocatedError enriches an error with a Location.
type LocatedError struct {
	Err      error
	Location Location
}

func (l *LocatedError) Unwrap() error {
	return l.Err
}

func (l *LocatedError) Error() string {
	return l.Err.Error()
}

// A DelimitedError enriches an error with a Delimitation.
type DelimitedError struct {
	Err          error
	Delimitation Delimitation
}

func (d *DelimitedError) Unwrap() error {
	return d.Err
}

func (d *DelimitedError) Error() string {
	return d.Err.Error()
}

// A Delimitation identifies a sequence of runes in a UTF-8 encoded text.
type Delimitation struct {
	Start, End Location
}

// A Delimitable provides a Delimitation.
type Delimitable interface {
	Delimit() Delimitation
}
