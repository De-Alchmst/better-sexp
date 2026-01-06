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

  (with-better-sexp
    (define (factorial n)
       (if (= n 0)
           1
           (* n : factorial : - n 1)))

    (define : foo x
      :: print x
      ::: x)

    :  print (factorial 5)
    :: print (foo 'hi)))
