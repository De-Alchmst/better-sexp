(module better-sexp (from-better-sexp)
  (import scheme (chicken base)
          srfi-1)

  (define bso 'better-sexp-open)
  (define bsc 'better-sexp-close)

  (define (from-better-sexp obj)
    (tokenize-better-sexp obj))
    ; (let ((tokens (tokenize-better-sexp obj)))
    ;   (let loop ((ts tokens))
    ;     (let ((head (car ts))
    ;           (tail (cdr ts)))

    ;       (cond
    ;         ((null? tail) '())

    ;         ((equal? head bso)
    ;          (cons (loop tail)
    ;                (loop tokens)))

    ;         ((equal? head bsc)
    ;          (set! tokens tail)
    ;          '())

    ;         (else
    ;          (cons head (loop tail))))))))
          

  (define (tokenize-better-sexp obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (cond
        ((null? tail)
         (if (not (pair? head))
             (list head bsc)
             (cons bso (tokenize-better-sexp head))))

        ((pair? head)
         (flatten bso (tokenize-better-sexp head)
                      (tokenize-better-sexp tail)))

        ((equal? head ':)
         (flatten bso (tokenize-better-sexp tail) bsc))

        ((equal? head '::)
         (flatten bsc bso (tokenize-better-sexp tail))) 

        ((equal? head ':::)
         ;; easier, since ': handles it's closing
         (flatten bso 'identity (tokenize-better-sexp tail) bsc))

        (else
         (cons head
               (tokenize-better-sexp tail)))))))
