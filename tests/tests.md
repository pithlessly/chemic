# Chemic Test Suite

This file contains a series of tests in the form of programs paired with their
expected output (either STDOUT or an error message). They can be run using the
target `make test` in the root directory, which compiles the project and then
runs a script which parses this file, testing each program against these
expectations (inspired by [Falderal](https://pypi.org/project/Falderal)).

Each test has an associated *description string* consisting of the name of the
sub-heading followed by its position in the list. For example, the first test
in this file has a description string `Arithmetic #1`.  If the environment
variable `CHEMIC_TEST_FILTER` is set, it will be interpreted as a regular
expression which tests will only be run if their description strings match.

## Arithmetic

They function correctly:

    > (display (+ 3 5))
    = 8

    > (display (- 0 100))
    = -100

    > (display (- 1))
    = -1

    > (display (* 5 5))
    = 25

Some operators work with no arguments:

    > (display (+))
    > (display (*))
    = 01

    > (display (-))
    ! compile error

Defer check to runtime:

    > (define negate -)
    > (display (negate))
    ! fatal error: wrong number of arguments: procedure expected at least 1, got 0

Operators fail if any intermediate computation overflows:

    > (+ 9223372036854775807 1 (- 1))
    ! fatal error: addition overflow

    > (+ 9223372036854775807 (- 1) 1)
    =

    > (* 9223372036854775807 2)
    ! fatal error: multiplication overflow

    > (- (- (- 9223372036854775807) 1))
    ! fatal error: negate underflow

## Syntax

An empty program does nothing:

    >
    =

Numeric literals are supported:

    > (display 5)
    = 5

Numeric literals cannot be larger than an `i64`:

    > 9223372036854775807
    =

    > 9223372036854775808
    ! compile error

Identifiers can contain weird characters:

    > (define !$%&*/:<=>?^_~+-.@ "test")
    > (display !$%&*/:<=>?^_~+-.@)
    = test

Identifiers can have leading digits:

    > (define 0_0 0)
