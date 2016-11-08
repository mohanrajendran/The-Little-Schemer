(load "Chapter-09-and-Again-and-Again-and-Again.ss")

;;
;; Entries
;;
;
; An entry is a pair of lists whose first list is a set. Also, both lists are of same length.
'((appetizer entree beverage)
  (pate boeuf vin))
'((appetizer entree beverage)
  (beer beer beer))
'((beverage dessert)
  ((food is) (number one with us)))
;
; We can create a new entry by aliasing the build function we had defined earlier:-
(define new-entry build)

;;
;; Lookup
;;
;
; Definition:-
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-in-entry-help
             name
             (cdr names)
             (cdr values)
             entry-f)))))
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))
(lookup-in-entry 'entree
                 '((appetizer entree beverage)
                   (food tastes good))
                 (lambda (x) x))              ;tastes

;;
;; Table
;;
;
; A table is a list of entries.
'(((appetizer entree beverage)
   (pate boeuf vin))
  ((beverage dessert)
   ((food is) (number one with us))))
;
; We can add a new entry to a table like this:-
(define extend-table cons)
;
; We can also lookup things in a table:-
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table
                                                     table-f))))))))
;
; Basically, we search an entry and pass in a function to search the rest of the table if
; the name is not found.
(lookup-in-table 'entree
                 '(((entree dessert)
                    (spaghetti spumoni))
                   ((appetizer entree beverage)
                    (food tastes good)))
                 (lambda (x) x))                ;spaghetti

;;
;; Quote
;;
(car (quote (a b c))) ;a
(cons 'a
      (cons 'b
            (cons 'c
                  (quote ())))) ;(a b c)
(cons 'car
      (cons (cons 'quote
                  (cons
                   (cons 'a
                         (cons 'b
                               (cons 'c
                                     (quote ()))))
                   (quote ())))
            (quote ())))        ;(car '(a b c))

(car (quote (a b c)))                 ;a

;;
;; Value
;;
;
; We are going to define a more general version of value function which has the following behaviour:-
(value (car (quote (a b c)))) ;a
;(value (quote (car (quote (a b c))))) ;(car '(a b c))
(value (add1 6))              ;7
(value 6)                     ;6
(value (quote nothing))       ;nothing
;(value ((lambda (nothing)
;          (cons nothing (quote ())))
;        (quote
;         (from nothing comes something)))) ;((from nothing comes something)
(value ((lambda (nothing)
          (cond
            (nothing (quote something))
            (else (quote nothing))))
        #t))

;;
;; Types
;;
;
; *const:-
6
#f
(value #f)
'cons
;
; Primitive:-
;(value car)
;
; *quote:-
(quote nothing)
;
; *identifier:-
; nothing
;
; *lambda:-
(lambda (x y) (cons x y))
;
; *application:-
((lambda (nothing)
   (cond
     (nothing (quote something))
     (else (quote nothing))))
 #t)
;
; *cond:-
;(cond
;  (nothing (quote something))
;  (else (quote nothing)))

;;
;; Evaluator
;;
;
; Note that the code is written in reverse order compared to the book for ease of evaluation:-
;
; Depending on the types of expressions, various actions can be taken(including environment table).
;
; For consts, we take the literal or treat it as a primitive
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))
;
; For the keyword quote, we simply evaluate to the second part of the list:-
(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))
;
; For identifiers, we retrieve them from the table:-
(define initial-table
  (lambda (name) (car (quote ()))))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
;
; For lambda, we store the existing table of free variables for closure and the arguments:-
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(*lambda '(lambda (x) (cons x y))
         '(((y z) ((8) 9))))      ;(non-primitive ((((y z) ((8) 9))) (x) (cons x y)))
;
; For cond, we take a list of cond-lines. If the question part on the left is false, it
; looks at the rest of the lines. Otherwise, it proceeds to answer the right part.
; If it encounters else, it executes the right side automatically.
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))
(define question-of first)
(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
;
; For application, we bind the given arguments to environment and execute the body:-

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))
(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((primitive? x) #t)
      ((non-primitive? x) #t)
      (else #f))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure) vals)
              (table-of closure)))))

(define *apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define *application
  (lambda (e table)
    (*apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

;
; We can decide which action to take basen on the tokens seen:-
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

;
; Finally, we can build the interpreter tying together the functions:-
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
;
; We start the evaluation with an empty environment:-
(define value
  (lambda (e)
    (meaning e (quote ()))))