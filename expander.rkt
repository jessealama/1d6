#lang br/quicklang

(require racket/contract)

(define (roll-value? x)
  (and (list? x)
       (not (empty? x))
       (andmap exact-positive-integer? x)))

(define (singleton-roll-value? x)
  (and (roll-value? x)
       (empty? (rest x))))

(define/contract (extract-one-value x)
  ((or/c exact-positive-integer?
         singleton-roll-value?)
   . -> .
   exact-positive-integer?)
  (match x
    [(? exact-positive-integer?)
     x]
    [(? list?)
     (first x)]))

(define/contract (random-roll num-sides)
  (exact-positive-integer? . -> . exact-positive-integer?)
  (if (= num-sides 1)
      1
      (random 1 num-sides)))

(define/contract (times-a-roll n d)
  ((or/c exact-positive-integer?
         singleton-roll-value?)
   roll-value?
   . -> .
   (and/c (listof exact-positive-integer?)
          (not/c empty?)))
  (define num-elements (extract-one-value n))
  (define num-sides (extract-one-value d))
  (map (lambda (x)
         (random-roll num-sides))
       (make-list num-elements 0)))

(define/contract (a-roll d)
  (exact-positive-integer? . -> . (list/c exact-positive-integer?))
  (list (random 1 d)))

(define-macro (1d6-module-begin PARSE-TREE)
  #`(#%module-begin
     (unsyntax-splicing #'PARSE-TREE)))

(provide (rename-out [1d6-module-begin #%module-begin]))

(define-macro (program STEPS ...)
  #'(STEPS ...))

(provide program)

(define-macro-cases term
  [(term "(" X ")")
   #'X]
  [(term (unary-operator "sum") X)
   #'(apply + X)]
  [(term X (binary-operator "+") Y)
   #'(+ (ensure-number X)
        (ensure-number Y))]
  [(term X (binary-operator "*") Y)
   #'(* (ensure-number X)
        (ensure-number Y))]
  [(term T)
   #'T])

(provide term)

(define roll-regexp #px"[dD]([1-9][0-9]*)")

(define-macro (roll-string->value R)
  #'(match (regexp-match roll-regexp R)
      [(list _ d)
       (list (random 1 (string->number d)))]))

(define-macro (scale->value S)
  #'(match S
      [(? exact-positive-integer?)
       S]
      [(list (? exact-positive-integer? x))
       x]))

(define-macro (ensure-number N)
  #'(match N
      [(? number?)
       N]
      [(list (? number? x))
       x]))

(define-macro-cases roll
  [(roll R)
   #'(roll-string->value R)]
  [(roll S R)
   #'(let ([scale-value (scale->value S)]
           [roll-value (roll-string->value R)])
       (times-a-roll scale-value roll-value))])

(provide roll)

(define-macro-cases scale
  [(scale "(" ROLL ")")
   #'ROLL])

(provide scale)

(define-macro (assignment IDENTIFIER ":=" R)
  (with-syntax ([name (format-id #'IDENTIFIER "~a" (syntax->datum #'IDENTIFIER))])
    #'(begin
        (define name R)
        (provide name))))

(provide assignment)

(define-macro (integer I)
  #'(string->number I))

(provide integer)

(define-macro (variable V)
  (format-id #'V "~a" (syntax->datum #'V)))

(provide variable)
