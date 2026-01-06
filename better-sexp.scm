(module better-sexp (with-better-sexp)
; (module better-sexp (parse-better-sexp tokenize-better-sexp)
  (import scheme (chicken base)
          (chicken syntax)
          srfi-1)

  (begin-for-syntax
    (define bso (gensym 'better-sexp-open))
    (define bsc (gensym 'better-sexp-close))

    (define (parse-better-sexp obj)
      (let ((tokens (tokenize-better-sexp obj)))
        (let loop ((ts tokens))
          (if (null? ts) '()
            (let ((head (car ts))
                  (tail (cdr ts)))

              (cond
                ((equal? head bso)
                 (cons (loop tail)
                       (loop tokens)))

                ((equal? head bsc)
                 (set! tokens tail)
                 '())

                (else
                 (cons head (loop tail)))))))))
            

    (define (tokenize-better-sexp obj)
      (let ((head (car obj))
            (tail (cdr obj)))
        (cond
          ((null? tail)
           (if (pair? head)
               (flatten bso (tokenize-better-sexp head) bsc)
               (list head)))

          ((pair? head)
           (flatten bso (tokenize-better-sexp head) bsc
                        (tokenize-better-sexp tail) bsc))

          ((equal? head ':)
           (flatten bso (tokenize-better-sexp tail) bsc))

          ((equal? head '::)
           (flatten bsc bso (tokenize-better-sexp tail))) 

          ((equal? head ':::)
           ;; easier, since ': handles it's closing
           (flatten bsc bso 'identity (tokenize-better-sexp tail)))

          (else
           (cons head
                 (tokenize-better-sexp tail)))))))

  
  (define-syntax with-better-sexp
    (er-macro-transformer
      (lambda (exp rename compare)
        (parse-better-sexp (cons 'begin (cdr exp)))))))
