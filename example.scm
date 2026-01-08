(module example ()
  (import scheme (chicken base)
          (chicken syntax)
          better-sexp)

  (with-better-sexp
    ;; a simple function-call chain
    (define (factorial n)
       (if (= n 0)
           1
           (* n : factorial : - n 1)))

    ;; prefix style
    (define : foo x
      :: print x
      ::: x)

    ;; semicolon style
    : print (factorial 5) ::
      newline ::
      print (foo 'hi) ::
      newline ::

      ;; it's a simple preprocessor, so quoted lists are also affected
      write '( A : B : C) :: newline)

  ;; as it's a macro, it cannot be applied on a runtime-generated list,
  ;; but it can be used with comile-time known lists
  (write (with-better-sexp
           '(foo : bar baz :: bax)))
  (newline))
