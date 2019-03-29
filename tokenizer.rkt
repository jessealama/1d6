#lang racket/base

(provide make-tokenizer
         tokenize-file)

(require brag/support
         racket/contract
         racket/port)

(module+ test
  (require rackunit))

(define (make-tokenizer port)
  (define (next-token)
    (define lexer
      (lexer-src-pos
       [(eof)
        eof]
       [whitespace
        (next-token)]
       [(from/to #\# #\newline)
        (next-token)]
       [(:: #\# (:* (:~ #\newline)))
        (next-token)]
       [(:: (union #\d #\D)
            (:/ #\1 #\9) (:* (:/ #\0 #\9)))
        (token 'ROLL lexeme)]
       [(:: (:/ #\1 #\9)
            (:* (:/ #\0 #\9)))
        (token 'INTEGER lexeme)]
       [(union "sum")
        (token lexeme lexeme)]
       [(:+ (:/ #\a #\z #\A #\Z))
        (token 'VARIABLE lexeme)]
       [(union ":="
               "+"
               "*"
               "-"
               "("
               ")")
        (token lexeme lexeme)]
       [any-char
        (token 'char lexeme)]))
    (lexer port))
  (port-count-lines! port)
  next-token)

(define/contract (tokenize-string str)
  (string? . -> . list?)
  (define (lex-it)
    (apply-tokenizer-maker make-tokenizer (current-input-port)))
  (with-input-from-string str lex-it))

(define/contract (tokenize-file path)
  (path-string? . -> . list?)
  (define (lex-it)
    (apply-tokenizer-maker make-tokenizer (current-input-port)))
  (with-input-from-file path lex-it #:mode 'text))

(module+ main

  (require racket/cmdline)

  (define file-to-process
    (command-line
     #:args (filename)
     filename))

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (tokenize-file file-to-process))
