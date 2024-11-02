package sources

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
