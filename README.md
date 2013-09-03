# persimmon

A sandbox for learning about parser implementation

This started out as a sibling project to
[lexluthor](https://github.com/spacemanaki/lexluthor), and was going
to be a yacc-style parser generator. There is still the early stages
of a yacc-clone (in src), but this has mutated a bit to be a
collection of little parsers and experiments with parser
implementation.

## of interest

Here are the more interesting bits:

1. a few simple recursive-descent parsers: [the babiest of them all, for matching opening and closing brackets](https://github.com/spacemanaki/persimmon/blob/master/recursive.sml), for [prefix arithmetic](https://github.com/spacemanaki/persimmon/blob/master/prefix.sml) and for [infix arithmetic](https://github.com/spacemanaki/persimmon/blob/master/pratt/recursive.sml).
2. a top-down operator-precedence style (aka Pratt) parser for [arithmetic expressions](https://github.com/spacemanaki/persimmon/blob/master/pratt/pratt.sml) and for [ML type expressions](https://github.com/spacemanaki/persimmon/blob/master/pratt/types.sml)

Feel free to get in touch if you have questions about this code. It
would be great if this was useful to someone else who was learning
about parsers!
