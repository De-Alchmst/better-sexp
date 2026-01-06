(module main ()
  (import scheme (chicken base)
          better-sexp)

  (write
    (from-better-sexp
      '(define (factorial n)
         (if (= n 0)
             1
             (* n (factorial (- n 1))))))))

