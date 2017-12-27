

#| Assignment 1 - Racket Query Language  (due Oct 4, 11:50pm, on Markus)

***Write the names, CDF accounts and student IDs for each of your group members below.***
***Max group size is two students.***
Patrick Vickery, vickeryp, vickeryp
|#
#lang racket

(provide attributes
         tuples
         size
         SELECT
         FROM
         WHERE
         ORDER)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.

  Attributes are first tuple so return head of table
|#
(define (attributes table)
  (first table))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.

  Non-attrube tuples are everything else
|#
(define (tuples table)
  (rest table))

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (- (length table) 1))


#|
  (common list1 list2)
  list1: a list of items
  list2: a list of items

  Returns the list of items that are in list1 and list2
|#
(define (common list1 list2)
  (filter (lambda (elem)
            (member elem list2))
          list1))
#|
  (all-common List)
  List: A List of lists

  Returns all of the elements that are in at least 2 of the lists nested in List
|#
(define (all-common List)
  (if (< (length List) 2)
      (list)
      (remove-duplicates
       (flatten
        (append (map
                 (lambda (x)
                   (common x (first List)))
                 (rest List))
                (all-common (rest List)))))))

#|
  (index-of list elem off)
  list: A list of elements
  elem: A element, that might be in list
  off: An offset for recursive calls. Defaults to 0

  Finds the index of elem in list.
|#
(define (index-of list elem [off 0])
  (if (empty? list)
      -1
      (if (eq? (first list) elem)
          off
          (index-of (rest list) elem (add1 off)))))

#|
  (rename current-attributes change-attributes new-name)
  current-attributes: A list of attributes
  change-attributes: Another list of attributes. This doesn't have to be a subset of current-attributes
  new-name: The prefix to add to any changed attributes

  Renames the current-atttibutes that are in the change-attributes using new name as a new prefix.
|#

(define (rename current-attributes change-attributes new-name)
  (if (or (empty? change-attributes) (empty? current-attributes))
      current-attributes
      (if (member (first current-attributes) change-attributes)
          (append (list (string-append new-name "." (first current-attributes) ))
                  (rename (rest current-attributes)
                          (remove (first current-attributes) change-attributes)
                          new-name))
          (cons (first current-attributes)
                (rename (rest current-attributes)
                        change-attributes
                        new-name)))))
#|
  (cross-product x y)
  x: A table
  y: Another table

  Takes the cross product of x and y
|#
(define (cross-product x y)
  (append (list( append (attributes x) (attributes y)))
          (map (lambda (i)
                 (flatten i))
               (cartesian-product (tuples x) (tuples y)))))

#|
  (all-cross-product L)
  L: A list of tables

  Takes the cross product of all tables in L
|#
(define (all-cross-product L)
  (if (= 1 (length L))
      (first L)
      (cross-product (first L) (all-cross-product (rest L)))))

#|
  (combine-columns c1 c2)
  c1: A column
  c2: Another column

  Returns a table by combining c1 and c2
|#

(define (combine-columns c1 c2)
  (map (lambda (row-index)
         (append (list-ref c1 row-index) (list-ref c2 row-index) ))
       (range (length c1))))

#|
  (combine-all-columns C)
  C: A list of columns

  Returns a table by combining all columns in C
|#
(define (combine-all-columns C)
  (if (= 1 (length C))
      (first C)
      (combine-columns (first C) (combine-all-columns (rest C)))))
      
                
#|
  (select-column attr table)
  attr: An attribute
  table: A table

  Returns a table with attr as the only attribute
|#
(define (select-column attr table)
  (map (lambda (row)
         (if (empty? attr)
             (list)
             (let ([index (index-of (attributes table) attr)])
               (if (= -1 index)
                   (list attr)
                   (list (list-ref row (index-of (attributes table) attr)) )))))
         table))

#|
  (select-columns list table)
  list: A list of attributes 
  table: A table

  Returns a table with list as the only attributes (in the order given)
|#
(define (select-columns list table)
  (if (list? list)
      (if (empty? list)
          (select-column list table)
          (if (= 1 (length list))
              (select-column (first list) table)
              (combine-columns (select-column (first list) table) (select-columns  (rest list) table))))
      (select-column list table)))
          


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (apply-attribute attributes attribute row)
  (if (member attribute attributes)
      (list-ref row (index-of attributes attribute))
      #f))

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (apply-f f table)
  (filter (lambda (elem)
            f(elem))
          table))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x attrs)
  (lambda (t)
    (if (member x attrs)
        (list-ref t (index-of attrs x))
        x)))


; ORDER helpers

#|
  (eq? a b)
  a: A number or a string
  b: Another number or string

  If they are of the same type, check if they are equal (with the corresponding function), otherwise false.
|#
(define (eq? a b)
  (if (and (number? a) (number? b))
      (= a b)
      (if (and (string? a) (string? b))
          (string=? a b)
          #f)))

#|
  (gt? a b)
  a: A number or a string
  b: Another number or string

  If they are of the same type, check a > b (with the corresponding function), otherwise let strings > numbers.
|#
(define (gt? a b)
  (if (and (number? a) (number? b))
      (> a b)
      (if (and (string? a) (string? b))
          (string>? a b)
          (if (string? a) ;and, implicitly, (number? b)
              #t
              #f))))

; ORDER BY
#|
  Macro to catch "ORDER <expr> <table>" and return <table> with the rows sorted by <expr>
|#
(define-syntax ORDER
  (syntax-rules ()
    [(_ <expr> <table>)
     (cons (attributes <table>)
           (sort (tuples <table>) #:key (replace <expr> (attributes <table>)) gt?))]))

; Starter for Part 3; feel free to ignore!

; What should this macro do?
; Replaces <atom> in <exprs> to be the corresponding element in <attrs>
; such that a meaningful expression can be returned by calling the function
(define-syntax replace
  (syntax-rules ()
    ;[(replace (<param1>) <attrs>) (replace <param1> <attrs>)]
    [(replace (<param> ...) <attrs>)
     (lambda (<tuple>)
       (if (empty? <tuple>)
           (list)
           (let ([s-expr (list ((replace <param> <attrs>)<tuple>) ...)])
             (apply (first s-expr) (rest s-expr)))))]
    ;(list ((replace <param> <attrs>)<tuple>) ...)))]
    [(replace <atom> <attrs>)
     (lambda (<tuple>)
       (if (empty? <tuple>)
           (list)
           ((replace-attr <atom> <attrs>) <tuple>)))]))


#|
  Macro to catch "WHERE <param> <table>" and return <table> only with rows satisfied by <param>
|#
(define-syntax WHERE
  (syntax-rules ()
    [(_ <param> <table>)
     (cons (attributes <table>)
           (filter (replace <param> (attributes <table>)) (tuples <table>) ))]))

#|
  Macro to catch "get-all-attr [<table1> <name1>] ..." and return a list of every attributes tuples from <table#>
|#
(define-syntax get-all-attr
  (syntax-rules ()
    [(_ [<table1> <name1>]) (list (attributes <table1>))]
    [(_ [<table1> <name1>] [<table2> <name2>] ...) (cons (attributes <table1>) (get-all-attr [<table2> <name2>] ...))]
    [(_ <table1>) (list <table1>)]))

#|
  Macro to catch "get-common-attr [<table1> <name1>] ..." and return a list of every attributes tuples from <table#>
|#
(define-syntax get-common-attr
  (syntax-rules()
    [(_ [<table1> <name1>] [<table2> <name2>] ...) (all-common (get-all-attr [<table1> <name1>] [<table2> <name2>] ...))]))

#|
  Macro to catch "rename-tables [<table1> <name1>] ..." and return a list of every table with all attributes renamed.
|#
(define-syntax rename-tables
  (syntax-rules ()
    [(_ common [<table1> <name1>])
     (list (cons (rename (attributes <table1>) common <name1>) (tuples <table1>)))]
    [(_ common [<table1> <name1>] [<table2> <name2>] ...)
     (cons (cons (rename (attributes <table1>) common <name1>) (tuples <table1>))
           (rename-tables common [<table2> <name2>] ...))]))

#|
  Macro to catch "FROM [<table1> <name1>] ..." and return a single table created by combining all tables and renaming them
|#
(define-syntax FROM
  (syntax-rules ()
    [(_ <table>) <table>]
    [(_ [<table1> <name1>] [<table2> <name2>] ...)
     (all-cross-product (rename-tables (get-common-attr [<table1> <name1>] [<table2> <name2>] ...)
                                       [<table1> <name1>] [<table2> <name2>] ...))]))

#|
  Macro to catch: "get-columns <attrs> <table>" or "get-columns * <table>"
  and get and combine the columns in the given order
|#
(define-syntax get-columns
  (syntax-rules (*)
    [(_ * <table>) <table>]
    [(_ <attrs> <table>) (select-columns <attrs> <table>)]))

#|
  Macro to catch:
    "SELECT <attrs> ... FROM [<table1> <name1>] ... " or
    "SELECT <attrs> ... FROM [<table1> <name1>] ... WHERE <pred>" or
    "SELECT <attrs> ... FROM [<table1> <name1>] ... ORDER BY <ord>" or
    "SELECT <attrs> ... FROM [<table1> <name1>] ... WHERE <pred> ORDER BY <ord>"  or

   Return a single table created by combining all tables, 
   renaming them, possibly filtering rows, and possibly sorting the rows
|#
(define-syntax SELECT
  (syntax-rules (* FROM WHERE ORDER BY)
    
    ; From -> Order -> Where -> Select
    [(_ <attrs> FROM <table1> <table2> ... WHERE <pred> ORDER BY <ord>)
     (get-columns <attrs> (WHERE <pred> (ORDER <ord> (FROM <table1> <table2> ...))))]
    
    ; From -> Where -> Select
    [(_ <attrs> FROM <table1> <table2> ... WHERE <pred>)
     (get-columns <attrs> (WHERE <pred> (FROM <table1> <table2> ...)))]

    ; From -> Order -> Select
    [(_ <attrs> FROM <table1> <table2> ... ORDER BY <ord>)
     (get-columns <attrs> (ORDER <ord> (FROM <table1> <table2> ...)))]

    ; From -> Select
    [(_ <attrs> FROM <table1> <table2> ... )
     (get-columns <attrs> (FROM <table1> <table2> ...))]))
  


