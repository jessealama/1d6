#lang racket

(provide read-syntax)

(require racket/contract
         (only-in (file "tokenizer.rkt")
                  make-tokenizer)
         (only-in (file "grammar.rkt")
                  parse))

(define (read-syntax path port)
  (datum->syntax
   #f
   `(module 1d6-module 1d6/expander
      ,(parse path (make-tokenizer port)))))
