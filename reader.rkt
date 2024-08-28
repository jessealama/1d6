#lang racket

(provide read-syntax)

(require racket/contract
         (file "tokenizer.rkt")
         (file "grammar.rkt"))

(define (read-syntax path port)
  (datum->syntax
   #f
   `(module 1d6-module 1d6/expander
      ,(parse path (make-tokenizer port)))))
