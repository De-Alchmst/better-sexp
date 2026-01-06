(module better-sexp (from-better-sexp)
  (import scheme (chicken base))

  (define (from-better-sexp obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
        ((null? tail)
         (list
           (if (not (pair? head))
               head
               (from-better-sexp head))))

        ((pair? head)
         (cons (from-better-sexp head)
               (from-better-sexp tail)))

        ((equal? head ':)
         (list (from-better-sexp tail)))

        (else
         (cons head
               (from-better-sexp tail)))))))

        
       
