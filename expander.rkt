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
  (map (lambda (x) (random-roll d))
       (make-list n 0)))

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

(define-macro (binary-product X Y)
  #'(* (ensure-number X)
       (ensure-number Y)))

(provide binary-product)

(define-macro (unary-sum T)
  #'(foldl + 0 T))

(provide unary-sum)

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

(define-macro (ensure-number N)
  #'(match N
      [(? number?)
       N]
      [(list (? number? x))
       x]))

(define (our-random n)
  (cond [(<= n 0)
         (error (format "Cannot make non-positive random numbers! Given ~a" n))]
        [(= n 1)
         1]
        [else
         (random 1 (add1 n))]))

(define-macro-cases roll
  [(_ N)
   #'(our-random N)]
  [(_ M N)
   #'(for/list ([i M])
       (our-random N))])

(provide roll)

(define-macro-cases scaled-roll
  [(_ (integer S) (integer M))
   #'(for/list ([i (in-range 1 S)])
       (our-random M))]
  [(_ (unscaled-roll R) M)
   #'(for/list ([i R])
       (our-random M))])

(provide scaled-roll)

(define-macro (unscaled-roll N)
  #'(our-random N))

(provide unscaled-roll)

(define-macro (assignment IDENTIFIER R)
  (with-syntax ([name (format-id #'IDENTIFIER "~a" (syntax->datum #'IDENTIFIER))])
    #'(begin
        (define name R)
        (provide name))))

(provide assignment)

(define-macro (integer I)
  #'I)

(provide integer)

(define-macro (variable V)
  (format-id #'V "~a" (syntax->datum #'V)))

(provide variable)

(define-macro (binary-mod A B)
  #'(modulo A B))

(provide binary-mod)

(define-macro (binary-quotient A B)
  #'(quotient A B))

(provide binary-quotient)

(define-macro (binary-difference A B)
  #'(- A B))

(provide binary-difference)

(define-macro (unary-maximum S)
  #'(cond [(list? S)
           (apply max S)]
          [(number? S)
           S]))

(provide unary-maximum)

(define-macro (unary-minimum S)
  #'(cond [(list? S)
           (apply min S)]
          [(number? S)
           S]))

(provide unary-minimum)
