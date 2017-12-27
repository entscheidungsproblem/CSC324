

#lang racket

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))


(define-syntax class
  (syntax-rules (__init__)
    [(class <Class>
       (__init__  (<attr> ...)
                  (<local> <Lvalue>)...
                  <init-body>)
       [(<method> <param> ...) <method-body>] ...)
     (define (<Class> <attr> ...)
       (begin
         <init-body>
         (let ([<local> <Lvalue>]...)
           (lambda (msg)
          
           
             (cond [(equal? msg (id->string <attr>)) <attr>]
                   ...
                   [(equal? msg (id->string <local>)) <Lvalue>]
                   ...
                   [(equal? msg (id->string <method>))
                    (lambda (<param> ...) <method-body>)]
                   ...
                   [else "Unrecognized message!"])))))]))
#|

First, here is an example of the class in use:

(class P (__init__ (x y)
                   (above_x_axis (< 0 y))
                   (write 'Created))
  [(right_of_y_axis)
   ((lambda()
      (< 0 x)))]
  [(distance other_point)
   ((lambda (other-point)
      (let ([dx (- x (other-point "x"))]
            [dy (- y (other-point "y"))])
        (sqrt (+ (* dx dx) (* dy dy)))))
    other_point)])

(define p1 (P -2 5))
(define p2 (P 2 -1))
((p1 "distance") p2)
(p1 "above_x_axis")
(p2 "above_x_axis")
((p1 "right_of_y_axis"))
((p2 "right_of_y_axis"))


The class tries to mimic the python style of defining a class.
The initializer first defines the arguments to construct an object.
Next, local values are defined by a name followed by a value, possibly found using function(s).
Finally, the initializer allows a body to be created to run some code upon creation of the object.
In the case above, a string was printed to confirm that the object is created.
Finally, methods are included as before.

|#


