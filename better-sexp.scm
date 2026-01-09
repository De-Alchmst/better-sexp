; MIT License

; Copyright (c) 2026 Đe-Alchmst

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(module better-sexp (with-better-sexp)
  (import scheme (chicken base)
          (chicken syntax)
          srfi-1)

  ;; begin-for-syntax needed to be usale inside macro transformer
  (begin-for-syntax
    ;; special symbols for opening and closing
    (define bso (gensym 'better-sexp-open))
    (define bsc (gensym 'better-sexp-close))
    (define bsv (gensym 'better-sexp-value))
            

    ;; takes a better-sexp and returns a flat list of it's tokenised
    ;; representation
    ;; uses bso/bsc to represent openings and closings lists
    ;; uses bsv to represent a literal value, which should be closed with bsc
    ;; for internal reasons
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

          ;; (::: x) : closes current list, opens bsv
          ;; bsv is a list of values, until closed with bsc
          ((equal? head ':::)
           (flatten bsc bsv (tokenize-better-sexp tail)))

          ;; (x y) : evaluates tail and conses head onto it
          (else
           (cons head
                 (tokenize-better-sexp tail))))))


    ;; takes a better-sexp, tokenizes it and parses the tokens back to a list
    ;; e.g.
    ;; ('begin bso 'print "hello" bsc bso '+ 1 2 bso '- 3 1 bsc bsc)
    ;; ==> (begin (print "hello") (+ 1 2 (- 3 1)))
    (define (parse-better-sexp obj)
      ;; loop is called for every bso encountered, and continues until
      ;; a matching bsc is found, at which point it returns the constructed
      ;; list and sets the rest of unevaluated tokens to the 'tokens' variable
      ;; this turned out to be less messy than dealing with values
      (define tokens (tokenize-better-sexp obj))
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

              ;; BSV : get a list from current tokens until matching BSC
              ;; but treat it as values instead  and append the result of
              ;; remaining tokens to it
              ((equal? head bsv)
               (append (loop tail)
                       (loop tokens)))

              ;; anything else: just cons it onto the result of
              ;; remaining tokens until matching BSC
              (else
               (cons head (loop tail)))))))))

  
  ;; takes a list of expressions using better-sexp syntax
  ;; and expands it into normal Scheme code
  (define-syntax with-better-sexp
    ;; insstead of syntax-rules pattern matching, er-macro-transformer
    ;; allows custom transformation logic
    (er-macro-transformer
      (lambda (exp rename compare)
        ;; replace with-better-sexp with begin before passing to the parser
        ;; this allows multiple expressions inside the with-better-sexp
        (parse-better-sexp (cons 'begin (cdr exp)))))))
