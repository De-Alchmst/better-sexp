(module better-sexp (with-better-sexp)
; (module better-sexp (parse-better-sexp tokenize-better-sexp)
  (import scheme (chicken base)
          (chicken syntax)
          srfi-1)

  ;; begin-for-syntax needed to be usale inside macro transformer
  (begin-for-syntax
    ;; special symbols for opening and closing
    (define bso (gensym 'better-sexp-open))
    (define bsc (gensym 'better-sexp-close))
            

    ;; takes a better-sexp and returns a flat list of it's tokenised
    ;; representation
    ;; uses bso/bsc to represent openings and closings lists
    ;; e.g.
    ;; (begin (print "hello") (+ 1 2 : - 3 1))
    ;; ==> ('begin bso 'print "hello" bsc bso '+ 1 2 bso '- 3 1 bsc bsc)
    (define (tokenize-better-sexp obj)
      (let ((head (car obj))
            (tail (cdr obj)))
        (cond
          ;; (x) : terminates list
          ((null? tail)
           (if (pair? head)
               (flatten bso (tokenize-better-sexp head) bsc)
               (list head)))

          ;; ((x) y) : evaluates both while enclosing x in a list
          ((pair? head)
           (flatten bso (tokenize-better-sexp head) bsc
                        (tokenize-better-sexp tail)))

          ;; (: x) : evaluates tail, enclosing it in a list
          ((equal? head ':)
           (flatten bso (tokenize-better-sexp tail) bsc))

          ;; (:: x) : closes current list, opens a new one and
          ;; evaluates tail inside it without closing
          ((equal? head '::)
           (flatten bsc bso (tokenize-better-sexp tail))) 

          ;; (::: x) : closes current list, opens a new one and
          ;; passes tail to identity function without closing
          ;; pass to identity makes code simpler, as I can have each case
          ;; handle it's own closing without interuptions from outside world
          ((equal? head ':::)
           (flatten bsc bso 'identity (tokenize-better-sexp tail)))

          ;; (x y) : evaluates tail and conses head onto it
          (else
           (cons head
                 (tokenize-better-sexp tail))))))

    ;; takes a better-sexp, tokenizes it and parses the tokens back to a list
    (define (parse-better-sexp obj)
      ;; loop is called for every bso encountered, and continues until
      ;; a matching bsc is found, at which point it returns the constructed
      ;; list and sets the rest of unevaluated tokens to the 'tokens' variable
      ;; this turned out to be less messy than dealing with values
      (let ((tokens (tokenize-better-sexp obj)))
        (let loop ((ts tokens))
          (if (null? ts) '() ;; exit at the end of tokens
            (let ((head (car ts))
                  (tail (cdr ts)))

              (cond
                ;; BSO : get a list from current tokens until matching BSC
                ;; and cons it onto result of remaining tokens
                ((equal? head bso)
                 (cons (loop tail)
                       (loop tokens)))

                ;; BSC : end current list and set remaining tokens
                ((equal? head bsc)
                 (set! tokens tail)
                 '())

                ;; anything else: just cons it onto the result of
                ;; remaining tokens until matching BSC
                (else
                 (cons head (loop tail))))))))))

  
  ;; takes a list of expressions using better-sexp syntax
  ;; and expands it into normal Scheme code
  (define-syntax with-better-sexp
    ;; insstead of syntax-rules pattern matching, er-macro-transformer
    ;; allows custom transformation logic
    (er-macro-transformer
      (lambda (exp rename compare)
        ;; replace with-better-sexp with begin befor passing to the parser
        ;; this allows multiple expressions inside the with-better-sexp
        (parse-better-sexp (cons 'begin (cdr exp)))))))
