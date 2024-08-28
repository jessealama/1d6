#lang racket/base

(provide make-tokenizer
         tokenize-file)

(require brag/support
         racket/contract
         racket/port
         racket/list)

(module+ test
  (require rackunit))

(define (is-d? char)
  (or (char=? char #\d)
      (char=? char #\D)))

(define (split-roll roll)
  (define characters (string->list roll))
  (define d-index (index-where characters is-d?))
  (define-values (left right)
    (split-at characters d-index))
  (cons (string->number (list->string left))
        (string->number (list->string (rest right)))))

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
       [(union ":="
               "+"
               "*"
               "-"
               "/"
               "("
               ")"
               "mod"
               "sum"
               "min"
               "max")
        (token (string->symbol lexeme) lexeme)]
       [(:: (union "d" "D"))
        (token 'D lexeme)]
       [(:+ (:/ #\0 #\9))
        (token 'INTEGER (string->number lexeme))]
       [(:+ (:/ #\a #\z #\A #\Z))
        (token 'VARIABLE lexeme)]
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

  (require racket/cmdline
           racket/pretty)

  (define file-to-process
    (command-line
     #:args (filename)
     filename))

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (pretty-print
   (tokenize-file file-to-process)))
