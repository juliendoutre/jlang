package sources

import "fmt"

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

type UnsupportedSchemeError struct {
	Scheme string
}

func (u *UnsupportedSchemeError) Error() string {
	return fmt.Sprintf("unsupported scheme %q", u.Scheme)
}

type DigestMismatchError struct {
	ExpectedDigest, ActualDigest []byte
}

func (d *DigestMismatchError) Error() string {
	return fmt.Sprintf("expected digest %x but got %x ", d.ExpectedDigest, d.ActualDigest)
}
