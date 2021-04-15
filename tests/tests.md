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

## Variables

`(set!)` can be used to modify global variables:

    > (define x 0)
    > (display x)
    > (set! x 1)
    > (display x)
    = 01

As well as local variables and parameters:

    > (let ((x 0))
    >   (define f
    >     (lambda (y)
    >       (display y)
    >       (set! y 1)
    >       (display x)
    >       (set! x y)))
    >   (display x)
    >   (display (f x))
    >   (display x))
    = 000()1

Changes to nonlocal variables by closures are still visible during the next call:

    > (define counter
    >   (lambda ()
    >     (define x 0)
    >     (lambda ()
    >       (set! x (+ x 1))
    >       x)))
    > (define c (counter))
    > (display (c))
    > (display (c))
    > (display (c))
    = 123

They are also visible to other closures:

    > (define getter-setter-generator
    >   (let ((x "foo"))
    >     (lambda ()
    >       (cons (lambda () x)
    >             (lambda (y) (set! x y))))))
    >
    > (define gs1 (getter-setter-generator))
    > (define gs2 (getter-setter-generator))
    > (display ((car gs1)))
    > ((cdr gs1) "bar")
    > (display ((car gs2)))
    > ((cdr gs2) "baz")
    > (display ((car gs1)))
    = foobarbaz

## Scope rules

This is one of the areas I had the most trouble with getting right. The spec
isn't entirely clear in some cases, and many of the implementations I've tested
differ in their behavior. The main reason for this is that, as a dynamic,
impure functional scripting language, a number of requirements are imposed on
Scheme which end up coming into tension with each other:

- It needs to permit side-effects at the top level. This means the order of
  top-level statements is potentially meaningful.

- It needs to facilitate recursive procedures. This means earlier functions
  need to be able to refer to later ones.

- It needs to permit global variables to be redefined. This is useful because
  everything in the standard library is dropped into scope by default, so it
  would be bad for additions to the standard library (e.g.
  implementation-specific functions) to break existing programs.

Haskell has only the second requirement. OCaml supports the first and the third
by default (only adjacent functions can be mutually recursive, and only using
the opt-in `and` keyword). Scheme needs to support all three, and that makes
things a bit awkward.

So accessing an undefined variable is illegal:

    > (display x)
    ! compile error

Accessing variables before they are defined syntactically is legal:

    > (define f (lambda () x))
    > (define x 10)
    > (display (f))
    = 10

    > (display (let ((_ 0))
    >            (define f (lambda () x))
    >            (define x 10)
    >            (f)))
    = 10

Accessing variables before they are defined at runtime is also legal (they are
initialized to nil):

    > (let ((a 0))
    >   (display b)
    >   (define b a))
    = ()

    > (define nil nil)
    > (display nil)
    = ()

`(define)` can only be used at the block level, but when its return value
is accessible, it is nil:

    > (display (define x 5))
    ! compile error

    > (let ((a (let ((b 0))
    >            (define _ _))))
    >   (display a))
    = ()

Initializing undefined variables to `nil` is only done for performance. Other
implementations initialize it to some special "uninitialized" singleton, or
just produce an error (but this compiling in a branch every time a global
variable is accessed).

Re-`(define)`ing a global variable at the top level overwrites it rather than
defining a new one:

    > (define x 0)
    > (define f (lambda () (display x)))
    > (f)
    > (define x 1)
    > (f)
    = 01

On the other hand, re-`(define)`ing a builtin procedure creates an entirely new
variable:

    > (define f (lambda () (display +)))
    > (f)
    > (define + 1)
    > (f)
    = ()1

Variables are always looked up in their most recent scope:

    > (define x 0)
    > (let ((x (+ 1 x)))
    >   (let ((x (+ 1 x)))
    >     (let ((x (+ 1 x)))
    >       (let ((x (+ 1 x)))
    >         (display x)
    >         (let ((x (+ 1 x)))
    >           (let ((x (+ 1 x)))
    >             (let ((x (+ 1 x)))
    >               (display x))))))))
    = 47

Variables defined by `(define)` take precedence over function parameters
and those defined by the RHS of a `(let)`:

    > ((lambda (x)
    >    (display x)
    >    (define x 0)) 0)
    = ()

    > (let ((x 0))
    >   (display x)
    >   (define x 0))
    = ()

## Arithmetic

Arithmetic operators function correctly:

    > (display (+ 3 5))
    = 8

    > (display (- 0 100))
    = -100

    > (display (- 1))
    = -1

    > (display (* 5 5))
    = 25

    > (display (< 0 1))
    > (display (< 0 (- 1)))
    > (display (< 0 0))
    = #t#f#f

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

## Cons cells

Cons cells can be constructed using `(cons)`:

    > (display (cons 1 2))
    = (1 . 2)

    > (define nil nil)
    > (display (cons 1 nil))
    = (1)

    > (define nil nil)
    > (display (cons 1 (cons 2 (cons 3 "()"))))
    > (display (cons 1 (cons 2 (cons 3 nil))))
    = (1 2 3 . ())(1 2 3)

Components of cons cells can be accessed using `car`, `cdr`, and their 2-, 3-,
and 4-compositions:

    > (define d display)
    > (define n "\n")
    > (define c (cons (cons (cons (cons "a" "b")
    >                             (cons "c" "d"))
    >                       (cons (cons "e" "f")
    >                             (cons "g" "h")))
    >                 (cons (cons (cons "i" "j")
    >                             (cons "k" "l"))
    >                       (cons (cons "m" "n")
    >                             (cons "o" "p")))))
    > (d c)
    > (d n)
    >
    > (d (car c)) (d (cdr c))
    > (d n)
    >
    > (d (caar c)) (d (cdar c)) (d (cadr c)) (d (cddr c))
    > (d n)
    >
    > (d (caaar c)) (d (cdaar c)) (d (cadar c)) (d (cddar c))
    > (d (caadr c)) (d (cdadr c)) (d (caddr c)) (d (cdddr c))
    > (d n)
    >
    > (d (caaaar c)) (d (cdaaar c)) (d (cadaar c)) (d (cddaar c))
    > (d (caadar c)) (d (cdadar c)) (d (caddar c)) (d (cdddar c))
    > (d (caaadr c)) (d (cdaadr c)) (d (cadadr c)) (d (cddadr c))
    > (d (caaddr c)) (d (cdaddr c)) (d (cadddr c)) (d (cddddr c))
    >
    = ((((a . b) c . d) (e . f) g . h) ((i . j) k . l) (m . n) o . p)
    = (((a . b) c . d) (e . f) g . h)(((i . j) k . l) (m . n) o . p)
    = ((a . b) c . d)((e . f) g . h)((i . j) k . l)((m . n) o . p)
    = (a . b)(c . d)(e . f)(g . h)(i . j)(k . l)(m . n)(o . p)
    = abcdefghijklmnop

Components of cons cells can be modified using `(set-car!)` and `(set-cdr!)`:

    > (define a! set-car!)
    > (define d! set-cdr!)
    > (define x (cons 1 2))
    > (a! x 3)
    > (display x)
    > (d! x 4)
    > (display x)
    = (3 . 2)(3 . 4)

These functions reveal that cons cells are passed by reference:

    > (define nil nil)
    > (define f
    >   (lambda (c)
    >     (set-cdr! c (cons "b" (cons "c" nil)))))
    > (let ((c (cons "a" nil)))
    >   (f c)
    >   (display c))
    = (a b c)

## Strings

String literals and heap-allocated strings both count:

    > (display (string? 0))
    > (display (string? string?))
    > (display (string? "abcd"))
    > (display (string? (string-copy "abcd")))
    = #f#f#t#t

Strings are displayed without surrounding quotes:

    > (display "abcd")
    = abcd

    > (display "a\nb")
    = a
    = b

Other operators:

    > (display (string-length "abcd"))
    > (display (string-length ""))
    = 40
