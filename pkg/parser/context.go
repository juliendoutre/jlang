package parser

func NewContext() Context {
	return Context{
		Sets:      map[string]Set{},
		Variables: map[string]Value{},
	}
}

type Context struct {
	Sets      map[string]Set
	Variables map[string]Value
}
