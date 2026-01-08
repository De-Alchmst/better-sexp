# Better S-Expressions

This is an experimental S-Expressions extension intended to make LISP a bit more 
readable (or at least have less parenthesis).

### New syntax

```
: opens a new list until it's parent list closes

   (foo : bar baz : bach)
    => (foo (bar baz (bax)))

:: Closes current list (including lists opened with :) and opens a new one
   in it's place

  (foo : bar baz :: bax)
  => (foo (bar baz) (bax))

  (write '(hello world) ::
   newline ::
   write (foo : bar) ::
   newline)
  => (write '(hello world))
     (newline)
     (write (foo (bar)))
     (newline)

::: Closes current list and puts whatever follows in as a single values.
    Can be followed by another ::: (no special effect) or :: (which in this case
    acts like :)

    (foo : bar ::: baz bax :: bach)
    ; same as (foo : bar ::: bar ::: bax :: bach)
    ; same as (foo : bar ::: bar bax : bach)
    => (foo (bar) baz bax (bach))

```

See [ecample.scm](./example.scm).

## See also

- [Wisp](https://srfi.schemers.org/srfi-119/srfi-119.html)
- [Sweet-expression](https://dwheeler.com/readable/)

Both good ideas, but I still want to have regular S-expressions available and I
don't fancy indentation-based blocks as much.
Sweet-expressions also try inplementing infix notation, which I don't really
need/want.
