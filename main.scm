(module main ()
  (import scheme (chicken base)
          (chicken syntax)
          better-sexp)

  ; (write
  ;   (parse-better-sexp
  ;     '(define (factorial n)
  ;        (if (= n 0)
  ;            1
  ;            (* n : factorial : - n 1)))))

  ; (newline)
  ; (newline)

  ; (write
  ;   (parse-better-sexp
  ;     '(define : foo x
  ;        :: bar x
  ;        :: baz x
  ;        ::: x)))

  (with-better-sexp
    (print : + 1 : * 2 3)
    (+ 1 2)
    (+ 1 2)
    (print "hello")))
