(load "Chapter-05-Oh-My-Gawd-ItsFull-Of-Stars.ss")

;;
;; Arithmetic expressions
;;
1
3
;cookie
;1 + 3
;1 + 3 * 4
;3 ^ y + 5
;
; For the purpose of this chapter, an erithmetic expression is either an atom
; (including numbers), or two arithmetic expressions combined by +, *, or ^.

;;
;; Quote
;;
(quote a)
(quote +)
(quote *)
(eq? (quote a) 'a) ;#t
(eq? 'a 'a)        ;#t
;
;(n + 3) is a representation for arithmetic expression n + 3. It can serve
;as an argument for a function.

;;
;; Numbered?
;;
;
;Definition:-
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))      
(numbered? 1)
;(3 + (4 * 5))
(numbered? '(3 + (4 ^ 5)))
;(numbered? (2 * sausage)) is not a number
;
;numbered? determines if a representaion of an arithmetic expression contains
;only numbers beside +, *, and ^.

;;
;; Value
;;
;
;Definition:-
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (o+ (value (car nexp))
           (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*)
       (x (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else
       (^ (value (car nexp))
          (value (car (cdr (cdr nexp)))))))))
(value 13)             ;13
(value '(1 + 3))       ;4
(value '(1 + (3 ^ 4))) ;82
;
;(value 'cookie) is invalid  since cookie is not numbered
;
;value returns the natural value of a valid numbered arithmetic expression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Seventh Commandment                              ;
;                                                      ;
; Recur on the subparts that are of the sae nature:    ;
; * On the subparts of a list.                         ;
; * On the subexpressions of an arithmetic expression. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; 3 + 4 can also be represented as (+ 3 4) of (plus 3 4) or (3 4 +)
; (+ (* 3 6) (^ 8 2))
;
;value redefined for prefix notation
(define value-pre
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+)
       (o+ (value-pre (car (cdr nexp)))
           (value-pre (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '*)
       (x (value-pre (car (cdr nexp)))
          (value-pre (car (cdr (cdr nexp))))))
      (else (^ (value-pre (car (cdr nexp)))
               (value-pre (car (cdr (cdr nexp)))))))))
;
; In certain cases, it might be easy to get the logic wrong and recur on the wrong
; part. It those cases, it makes sense to use helper functions to abstract away the
; parts we might get wrong.
;
; 1st-sub-exp
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
;
; 2nd-sub-exp
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
;
; operator
(define operator
  (lambda (aexp)
    (car aexp)))
;
; With the new helpers, we can re-write the value function
(define value-pre
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (o+ (value-pre (1st-sub-exp nexp))
           (value-pre (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*)
       (x (value-pre (1st-sub-exp nexp))
          (value-pre (2nd-sub-exp nexp))))
      (else
       (^ (value-pre (1st-sub-exp nexp))
          (value-pre (2nd-sub-exp nexp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Eighth Commandment                               ;
;                                                      ;
; Use help functions to abstract from representations. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Representing numbers using lists
;;
;
; Like how we built an idea of numbers using number?, zero?, add1, sub1 and 0,
; we can use alternate representations for numbers. Lets use () to represent zero
;
; () => 0
; (()) => 1
; (()()) => 2
; (()()()) => 3
;
; Test for zero
(define sero?
  (lambda (n)
    (null? n)))
;
; Adding one
(define edd1
  (lambda (n)
    (cons '() n)))
;
; Subtracting one
(define zub1
  (lambda (n)
    (cdr n)))
;
; zub1 is undefined for () like how sub1 was undefined for 0
;
; Defining addition
(define l+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (l+ n (zub1 m)))))))

;
; Tuples
; (1 2 3) can be redefined as ((()) (()()) (()()()))