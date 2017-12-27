

#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#
(define (subsets list)
  (if (empty? list)
      list
      (list-<(combinations list))))

(define (list-< list)
  (cond [(equal? (length list) 1) (first list)]
        [else (-< (first list) (list-< (rest list)))]))


; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#

(define (transpose puzzle)
  (apply map list puzzle))

(define (check-rows puzzle)
  (if (empty? puzzle)
      #t
      (if (not (equal? (list->set (first puzzle)) (list->set(list 1 2 3 4))))
          #f
          (check-rows (rest puzzle)))))

(define (check-columns puzzle)
  (check-rows (transpose puzzle)))    

; Convert to list of lists of elements in boxes
(define (get-boxes puzzle)
  (let ([r1 (first puzzle)]
        [r2 (list-ref puzzle 1)]
        [r3 (list-ref puzzle 2)]
        [r4 (last puzzle)])
    (append
     (list (append (drop-right r1 2) (drop-right r2 2)))
     (list (append (take-right r1 2) (take-right r2 2)))
     (list (append (drop-right r3 2) (drop-right r4 2)))
     (list (append (take-right r3 2) (take-right r4 2))))))
           
  
(define (check-boxes puzzle)
  (check-rows (get-boxes puzzle)))

(define (check-sudoku puzzle)
  (and (check-rows puzzle) (check-columns puzzle) (check-boxes puzzle)))

(define (generate-potential puzzle)
  (map (lambda (row)
         (map (lambda (elem)
                (if (equal? elem "")
                    (-< 1 2 3 4)
                    elem))
              row))
       puzzle))

(define (sudoku-4 puzzle)
  (?- check-sudoku (generate-potential puzzle)))

; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    [(_ <combine> <init> <expr>)
     (foldl <combine> <init> (all <expr>))]))


