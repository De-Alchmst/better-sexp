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

:::

```

## TODO

`:::` is currently implemented by wrapping it's right side in a `identity`
function.
This works in evaluated code, but is not usable in quoted list construction.
Make it not do that.

## See also

- [Wisp](https://srfi.schemers.org/srfi-119/srfi-119.html)
- [Sweet-expression](https://dwheeler.com/readable/)

Both good ideas, but I still want to have regular S-expressions available and I
don't fancy indentation-based blocks as much.
Sweet-expressions also try inplementing infix notation, which I don't really
need/want.
