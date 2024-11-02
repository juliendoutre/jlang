package sources

// A Location identifies a specific rune in a UTF-8 encoded text.
type Location struct {
	Line, Column uint
}

// A Delimitation identifies a sequence of runes in a UTF-8 encoded text.
type Delimitation struct {
	Start, End Location
}

// A Delimitable provides a Delimitation.
type Delimitable interface {
	Delimit() Delimitation
}
