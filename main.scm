(module main ()
  (import scheme (chicken base)
          better-sexp)

  (write
    (from-better-sexp
      '(define (factorial n)
         (if (= n 0)
             1
             (* n : factorial : - n 1)))))

  (newline)
  (newline)

  (write
    (from-better-sexp
      '(define : foo x
         :: bar x
         :: baz x
         ::: x))))

