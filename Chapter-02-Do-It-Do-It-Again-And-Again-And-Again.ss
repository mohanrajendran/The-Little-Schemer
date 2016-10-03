(load "Chapter-01-Toys.ss")

;;
;; List of atoms
;;
;
; Definition:-
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(lat? '(Jack Sprat could eat no chicken fat))   ;#t
(lat? '((Jack) Sprat could eat no chicken fat)) ;#f
(lat? '(Jack (Sprat could) eat no chicken fat)) ;#f
(lat? '())                                      ;#t
;
; lat? is used to check if an argument is a list of atoms
;
; (cond ...) asks questions
; (lambda ...) creates a function
; (define ...) gives it a name
(lat? '(bacon and eggs)) ;#t
; Goes through (atom? 'bacon) => (atom? 'and) => (atom? 'eggs) => (null? '()) => #t
(lat? '(bacon (and eggs))) ;#f
; Goes through (atom? 'bacon) => (atom? '(and eggs)) => (else #f) => #f
;
; else is always true when used in cond. It is used to define default behavior when no question in cond evaulated to #t

;;
;; Or
;;
(or (null? '()) (atom? '(d e f g))) ;#t
(or (null? '(a b c)) (null? '()))   ;#t
(or (null? '(abc)) (null? '(atom))) ;#f
;
; (or ...) ask two questions, one at a time. If the first question answers true, it stops right there.
; Otherwise, it evaluates to the second question

;;
;; Member?
;;
;
; Defintion:-
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
(member? 'tea '(coffee tea or milk))                ;#t
(member? 'poached '(fried eggs and scrambled eggs)) ;#f
(member? 'meat '(mashed potatoes and meat gravy))   ;#t
;
; goes through (eq? 'mashed 'meat) => (member? '(potatoes and meat gravy) 'and) => ...
; => (or (eq? 'meat 'meat) (member? '(gravy))) => (or #t ...) => #t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The First Commandment (preliminary)                  ;
;                                                      ;
; Always ask null? as the first question in expressing ;
; any function.                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(member? 'liver '(bagels and lox)) ;#f
;
; goes through (eq? 'liver 'bagels) => ... => (or (eq? 'liver 'lox) (member? 'liver '()))
; => (or #f (member? 'liver '())) => (member? 'liver '()) => (null? '()) => #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; THIS SPACE USED FOR DOODLING ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

