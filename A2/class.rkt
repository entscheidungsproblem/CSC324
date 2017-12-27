

#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait)

; QUESTION 1 (metaprogramming).
(define-syntax class-meta
  (syntax-rules ()
    [(_ <Class> (<attr> ...)
        [(<method> <param> ...) <body>]...)
     (define (<Class> <attr> ...)
       (let ([<M> "Unrecognized message!"]
             [<_attr> (list (list (id->string <attr>) <attr>) ...)]
             [<_method> (list (list (id->string <method>)
                                    (lambda (<param> ...)
                                      <body>))...)])
         
         (lambda (msg)
           (cond [(equal? msg "_attributes")
                  (sort <_attr> #:key car string<=?)]
                 [(equal? msg "_methods")
                  (sort <_method> #:key car string<=?)]
                 [(equal? msg (id->string <attr>)) <attr>]
                 ...
                 [(equal? msg (id->string <method>))
                  (lambda (<param> ...) <body>)]
                 ...
                 [else <M>]))))]))




(define-syntax class-trait
  (syntax-rules (with)
    [(_ <Class> (<attr> ...) (with)
        [(<method> <param> ...) <body>]...)
     (class-meta <Class> (<attr> ...)
        [(<method> <param> ...) <body>]...)]
    [(_ <Class> (<attr> ...) (with <trait> <traits> ...)
        [(<method> <param> ...) <body>]...)
     (define (<Class> <attr> ...)
       (begin (class-trait temp (<attr> ...) (with <traits> ...)
                           [(<method> <param> ...) <body>]...)
              (lambda (msg)
                ((<trait> (temp <attr> ...))msg))))]))

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))


