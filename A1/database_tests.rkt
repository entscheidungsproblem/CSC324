

#| Assignment 1 - Racket Query Language Tests (due Oct 4, 11:50pm on Markus)

***Write the names, CDF accounts and student id for each of your group members below.***
***Max group size is two students.***
<Name>, <CDF>, <ID>
Patrick Vickery, <CDF>, 1000799781
|#

; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)

; This imports your file; do not change this line!
(require "database.rkt")

(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))

; student created tables

(define Food
  '(("Name" "IsChocolate") 
    ("Aero" #t) 
    ("Rice" #f) 
    ("Kit-Kat" #t)))

(define Chocolate
  '(("Name" "Calories")
    ("Kit-Kat" 230)
    ("Aero" 200)
    ("Reese" 300)
    ("Caramilk" 290)))

(define Empty
  '(()))

(define Empty2
  '(("Anything")))

#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----
; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select given a literal table
(test
 (SELECT '("A" "B")
         FROM '(("C" "A" "B" "D")
                (1 "Hi" 5 #t)
                (2 "Bye" 5 #f)
                (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))

; Select all from two product of two tables
(test (SELECT * FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #T "David" "CSC343")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")))

; Select some from two tables
(test (SELECT '("P.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Jen" "CSC324" 30)
        ("Jen" "CSC108" 30)
        ("Jen" "CSC343" 30)
        ("Paul" "CSC324" 100)
        ("Paul" "CSC108" 100)
        ("Paul" "CSC343" 100)))

; Take the product of a table with itself
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E"])
      '(("E.Course" "E1.Course")
        ("CSC324" "CSC324")
        ("CSC108" "CSC324")
        ("CSC343" "CSC324")
        ("CSC324" "CSC108")
        ("CSC108" "CSC108")
        ("CSC343" "CSC108")
        ("CSC324" "CSC343")
        ("CSC108" "CSC343")
        ("CSC343" "CSC343")))

; Take the product of a literal table with an identifier
(test
 (SELECT *
         FROM ['(("Age" "A" "Name" "D")
                 (1 "Hi" 5 #t)
                 (2 "Bye" 5 #f)
                 (3 "Hi" 10 #t))
               "T1"]
         [Person "T2"])
 '(("T1.Age" "A" "T1.Name" "D" "T2.Name" "T2.Age" "LikesChocolate")
   (1 "Hi" 5 #t "David" 20 #t)
   (1 "Hi" 5 #t "Jen" 30 #t)
   (1 "Hi" 5 #t "Paul" 100 #f)
   (2 "Bye" 5 #f "David" 20 #t)
   (2 "Bye" 5 #f "Jen" 30 #t)
   (2 "Bye" 5 #f "Paul" 100 #f)
   (3 "Hi" 10 #t "David" 20 #t)
   (3 "Hi" 10 #t "Jen" 30 #t)
   (3 "Hi" 10 #t "Paul" 100 #f)))

; Select from Empty
(test (SELECT * FROM Empty)
      '(()))

; Select from Empty
(test (SELECT "Anything" FROM Empty)
      '(("Anything")))

; Select from Empty2
(test (SELECT "Anything" FROM Empty2)
      '(("Anything")))

; Mix attributes from a duplicated table
(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"])
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age")
        (20 "David" #t 20)
        (20 "Jen" #t 30)
        (20 "Paul" #t 100)
        (30 "David" #t 20)
        (30 "Jen" #t 30)
        (30 "Paul" #t 100)
        (100 "David" #f 20)
        (100 "Jen" #f 30)
        (100 "Paul" #f 100)))

         

; Cross with an empty table with no attributes
(test (SELECT * FROM [Person "P"] ['(())' "P2"])
      '(("Name" "Age" "LikesChocolate")))

;Cross with an empty table with no attributes but an empty tuple
(test (SELECT "Age" FROM [Person "P"] ['(() ())' "P2"])
      '(("Age") (20) (30) (100)))

;Cross with an empty table with no attributes but 2 empty tuples
(test (SELECT * FROM [Person "P"] ['(() () ())' "P2"])
      '(("Name" "Age" "LikesChocolate") ("David" 20 #t) ("David" 20 #t) ("Jen" 30 #t) ("Jen" 30 #t) ("Paul" 100 #f) ("Paul" 100 #f)))

(test (SELECT * FROM [Person "P"] ['(("Name") () ())' "P2"])
      '(("P.Name" "Age" "LikesChocolate" "P2.Name")
        ("David" 20 #t)
        ("David" 20 #t)
        ("Jen" 30 #t)
        ("Jen" 30 #t)
        ("Paul" 100 #f)
        ("Paul" 100 #f)))

(test (SELECT ".Name" FROM [Person "P"] [Person ""])
      '((".Name") ("David") ("Jen") ("Paul") ("David") ("Jen") ("Paul") ("David") ("Jen") ("Paul")))

(test (SELECT '("Calories" "Name") FROM Chocolate)
      '(("Calories" "Name") (230 "Kit-Kat") (200 "Aero") (300 "Reese") (290 "Caramilk")))

(test (SELECT * FROM ['(("test") (1) (2))' "X"] ['(("test") (1) (2))' "Y"] ['(("test2") (1) (2))' "Z"])
      '(("X.test" "Y.test" "test2") (1 1 1) (1 1 2) (1 2 1) (1 2 2) (2 1 1) (2 1 2) (2 2 1) (2 2 2)))

(test (SELECT "Job" FROM Person)
      '(("Job") ("Job") ("Job") ("Job")))


; ---- WHERE ----
; Attribute as condition, select all
(test (SELECT *
              FROM Person
              WHERE "LikesChocolate")
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)))

; Attribute as condition, select subset
(test (SELECT '("LikesChocolate" "Name")
              FROM Person
              WHERE "LikesChocolate")
      '(("LikesChocolate" "Name")
        (#t "David")
        (#t "Jen")))

; Condition as function of one attribute, select all
(test (SELECT *
              FROM Person
              WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select none
(test (SELECT '()
              FROM Teaching
              WHERE (equal? "Name" "David"))
      '(()
        ()
        ()))

; Constant true condition
(test (SELECT *
              FROM Person
              WHERE #t)
      Person)

; Constant false compound condition
(test (SELECT *
              FROM Person
              WHERE (> (string-length "David") 20))
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table
(test (SELECT '("C" "B")
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))

; Simple condition on joined tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              WHERE (equal? "P.Name" "T.Name"))
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "David" "CSC343")
        ("Paul" 100 #f "Paul" "CSC108")))


; Compound condition on three joined tables
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
              FROM [Person "P"] [Teaching "T"] [Person "P1"]
              WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))


; Mix attributes from a duplicated table
(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE (= "P.Age" "P2.Age"))
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") (20 "David" #t 20) (30 "Jen" #t 30) (100 "Paul" #f 100)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE #t)
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age")
        (20 "David" #t 20)
        (20 "Jen" #t 30)
        (20 "Paul" #t 100)
        (30 "David" #t 20)
        (30 "Jen" #t 30)
        (30 "Paul" #t 100)
        (100 "David" #f 20)
        (100 "Jen" #f 30)
        (100 "Paul" #f 100)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE (If (string=? "P.Name" "P2.Name") (= "P.Age" "P2.Age") #f))
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") (20 "David" #t 20) (30 "Jen" #t 30) (100 "Paul" #f 100)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE (If (And (string=? "P.Name" "P2.Name") (= "P.Age" "P2.Age")) (< "P.Age" 50) (> "P.Age" 50)))
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") (20 "David" #t 20) (30 "Jen" #t 30) (100 "David" #f 20) (100 "Jen" #f 30)))

(test (SELECT * FROM [Person "P"] ['(("Name") () ())' "P2"] WHERE (< "Age" (- 100 "Age")))
      '(("P.Name" "Age" "LikesChocolate" "P2.Name") ("David" 20 #t) ("David" 20 #t) ("Jen" 30 #t) ("Jen" 30 #t)))

(test (SELECT * FROM Person WHERE (= (+ (string-length "Name") "Age") 25))
      '(("Name" "Age" "LikesChocolate") ("David" 20 #t)))

(test (SELECT ".Name" FROM [Person "P"] [Person ""] WHERE (= 1 0))
      '((".Name")))

(test (SELECT '("Calories" "Name") FROM Chocolate WHERE (Or (= "Calories" 200) (string=? "Name" "Reese")))
      '(("Calories" "Name") (200 "Aero") (300 "Reese")))

(test (SELECT * FROM ['(("test") (1) (2))' "X"] ['(("test") (1) (2))' "Y"] ['(("test2") (1) (2))' "Z"] WHERE (And (Or (= "X.test" 1) (= "Y.test" 1)) (> "test2" 1)))
      '(("X.test" "Y.test" "test2") (1 1 2) (1 2 2) (2 1 2)))

; ---- ORDER BY ----
; Order by attribute
(test (SELECT *
              FROM Person
              ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by attribute, not selected
(test (SELECT '("Name")
              FROM Person
              ORDER BY "Age")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; Order by a function of an attribute
(test (SELECT *
              FROM Person
              ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Paul" 100 #f)
        ("Jen" 30 #t)))

; Order with duplicate
(test (SELECT *
              FROM Teaching
              ORDER BY (+ (string-length "Name") (string-length "Course")))
      '(("Name" "Course")
        ("David" "CSC324")
        ("David" "CSC343")
        ("Paul" "CSC108")))

; Order on a literal table
(test (SELECT *
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              ORDER BY "C")
      '(("A" "B" "C")
        (3 10 40)
        (4 4 4)
        (1 2 3)
        (2 3 -1)))

; Order on two tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))



; Mix attributes from a duplicated table
(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] ORDER BY "P.Age")
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age")
        (100 "David" #f 20)
        (100 "Jen" #f 30)
        (100 "Paul" #f 100)
        (30 "David" #t 20)
        (30 "Jen" #t 30)
        (30 "Paul" #t 100)
        (20 "David" #t 20)
        (20 "Jen" #t 30)
        (20 "Paul" #t 100)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] ORDER BY (+ (string-length "P2.Name") "P.Age"))
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age")
        (100 "David" #f 20)
        (100 "Paul" #f 100)
        (100 "Jen" #f 30)
        (30 "David" #t 20)
        (30 "Paul" #t 100)
        (30 "Jen" #t 30)
        (20 "David" #t 20)
        (20 "Paul" #t 100)
        (20 "Jen" #t 30)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] ORDER BY (abs(- "P.Age" (string-length "P.Name"))))
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age")
        (100 "David" #f 20)
        (100 "Jen" #f 30)
        (100 "Paul" #f 100)
        (30 "David" #t 20)
        (30 "Jen" #t 30)
        (30 "Paul" #t 100)
        (20 "David" #t 20)
        (20 "Jen" #t 30)
        (20 "Paul" #t 100)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] ORDER BY "P2.Name")
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age")
        (20 "Paul" #t 100)
        (30 "Paul" #t 100)
        (100 "Paul" #f 100)
        (20 "Jen" #t 30)
        (30 "Jen" #t 30)
        (100 "Jen" #f 30)
        (20 "David" #t 20)
        (30 "David" #t 20)
        (100 "David" #f 20)))

(test (SELECT * FROM [Person "P"] ['(("Name") () ())' "P2"] ORDER BY (+ "Age" (string-length "P.Name")))
      '(("P.Name" "Age" "LikesChocolate" "P2.Name")
        ("Paul" 100 #f)
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("Jen" 30 #t)
        ("David" 20 #t)
        ("David" 20 #t)))

(test (SELECT * FROM Person ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate") ("David" 20 #t) ("Paul" 100 #f) ("Jen" 30 #t)))

(test (SELECT '("Calories" "Name") FROM Chocolate ORDER BY (- 0 "Calories"))
      '(("Calories" "Name") (200 "Aero") (230 "Kit-Kat") (290 "Caramilk") (300 "Reese")))

(test (SELECT * FROM ['(("test") (1) (2))' "X"] ['(("test") (1) (2))' "Y"] ['(("test2") (1) (2))' "Z"] ORDER BY (+ "X.test" "Y.test" (- 0 "test2")))
      '(("X.test" "Y.test" "test2") (2 2 1) (1 2 1) (2 1 1) (2 2 2) (1 1 1) (1 2 2) (2 1 2) (1 1 2)))


; ---- ORDER BY and WHERE ----
; Use attributes, select all 
(test
 (SELECT * 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name" "Age" "LikesChocolate")
   ("Jen" 30 #t)
   ("David" 20 #t)))

; Use attributes, select one unused attribute
(test
 (SELECT '("Name") 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name")
   ("Jen")
   ("David")))

; Two joined tables, select all
(test
 (SELECT * 
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Two joined tables, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))



; Mix attributes from a duplicated table
(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE (= "P.Age" "P2.Age") ORDER BY "P.Age")
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") (100 "Paul" #f 100) (30 "Jen" #t 30) (20 "David" #t 20)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE #t ORDER BY (+ (string-length "P2.Name") "P.Age"))
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age")
        (100 "David" #f 20)
        (100 "Paul" #f 100)
        (100 "Jen" #f 30)
        (30 "David" #t 20)
        (30 "Paul" #t 100)
        (30 "Jen" #t 30)
        (20 "David" #t 20)
        (20 "Paul" #t 100)
        (20 "Jen" #t 30)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE (If (string=? "P.Name" "P2.Name") (= "P.Age" "P2.Age") #f) ORDER BY (abs(- "P.Age" (string-length "P.Name"))))
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") (100 "Paul" #f 100) (30 "Jen" #t 30) (20 "David" #t 20)))

(test (SELECT '("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") FROM [Person "P"] [Person "P2"] WHERE (If (And (string=? "P.Name" "P2.Name") (= "P.Age" "P2.Age")) (< "P.Age" 50) (> "P.Age" 50)) ORDER BY "P2.Name")
      '(("P.Age" "P2.Name" "P.LikesChocolate" "P2.Age") (30 "Jen" #t 30) (100 "Jen" #f 30) (20 "David" #t 20) (100 "David" #f 20)))

(test (SELECT * FROM [Person "P"] ['(("Name") () ())' "P2"] WHERE (< "Age" (- 100 "Age")) ORDER BY (+ "Age" (string-length "P.Name")))
      '(("P.Name" "Age" "LikesChocolate" "P2.Name") ("Jen" 30 #t) ("Jen" 30 #t) ("David" 20 #t) ("David" 20 #t)))

(test (SELECT * FROM Person WHERE (= (+ (string-length "Name") "Age") 25) ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate") ("David" 20 #t)))

(test (SELECT '("Calories" "Name") FROM Chocolate WHERE (Or (= "Calories" 200) (string=? "Name" "Reese")) ORDER BY (- 0 "Calories"))
      '(("Calories" "Name") (200 "Aero") (300 "Reese")))

(test (SELECT * FROM ['(("test") (1) (2))' "X"] ['(("test") (1) (2))' "Y"] ['(("test2") (1) (2))' "Z"] WHERE (And (Or (= "X.test" 1) (= "Y.test" 1)) (> "test2" 1)) ORDER BY (+ "X.test" "Y.test" (- 0 "test2")))
      '(("X.test" "Y.test" "test2") (1 2 2) (2 1 2) (1 1 2)))

; ---- Nested queries ----
(test
 (SELECT * 
         FROM (SELECT '("Age" "Name") FROM Person))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

(test
 (SELECT '("Person.Name" "Course")
         FROM [(SELECT '("Name") FROM Person) "Person"]
         [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                            (equal? "Course" "CSC108")))
          "Teaching"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; Nested query containing a literal
(test
 (SELECT *
         FROM [(SELECT '("A") 
                       FROM '(("A" "B") 
                              (1)
                              (10)))
               "Table1"]
         [(SELECT *
                  FROM '(("C" "A")
                         ("Hi" "Bye")
                         ("Dog" "Cat")
                         ("Red" "Blue")))
          "Table2"]
         WHERE (And (equal? (string-length "Table2.A") 3) (< 0  "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))

(test
 (SELECT '("A.Z" "B.W" "M" "N" "Q" "X" "Y") FROM 
         [(SELECT '("Z" "X" "Y") FROM 
                  '(("X" "Y" "Z")
                    (10 16 19)
                    (12 15 17)
                    (11 16 18)
                    (140 1 2)
                    (10 0 23)) 
                  WHERE (> "Y" 1)) "A"]
         [(SELECT '("Z" "W" "Q") FROM 
                  '(("W" "Q" "Z")
                    (1 5 19)
                    (1 5 17)
                    (1 2 1)
                    (10 1 2))
                  WHERE (= "Q" 5)) "B"]
         [(SELECT '("W" "M" "N") FROM 
                  '(("W" "N" "M")
                    (1 0 1)
                    (2 1 7)
                    (1 8 1)
                    (10 6 7))) "C"]
         WHERE (And (= "A.Z" "B.Z") (= "B.W" "C.W"))
         ORDER BY (+ "M" "N"))
 '(("A.Z" "B.W" "M" "N" "Q" "X" "Y") (19 1 1 8 5 10 16) (17 1 1 8 5 12 15) (19 1 1 0 5 10 16) (17 1 1 0 5 12 15)))


