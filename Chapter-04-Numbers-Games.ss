(load "Chapter-03-Cons-the-Magnificent.ss")

;;
;; Numbers
;;
14
(atom? 14) ;#t
;
; All numbers are atoms
-3
3.14159
;
; -3 and 3.14159 are technically numbers. But only non-negative
; integers are considered in this chapter.

;;
;; Add1
;;
;
; Definition:-
(define add1
  (lambda (n)
    (+ n 1)))

(add1 67) ;68

;;
;; Sub1
;;
;
; Defintion:-
(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5) ;4
;
; (sub1 0) is not valid, because the result, -1 is outside the region of interest

;;
;; Zero?
;;
(zero? 0) ;#t

;;
;; o+
;;
;
; Definition:-
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
(o+ 46 12) ;58
;
; We can treat zero? like null? and add1 like cons.
; zero? asks if a number is empty and add1 builds numbers.

;;
;; o-
;;
;
; Definition:-
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))
(o- 14 3) ;11
(o- 17 9) ;8
;
; (o- 18 25) is invalid bacause -8 is negative.
;
; o- takes two number as arguments and reduces the second until
; it reaches zeo. It subtracts one from the the first argument
; as many times as it did for the second argument to reach zero.

;;
;; Tuples
;;
'(2 11 3 79 47 6)
'(8 55 5 555)
;
; '(1 2 8 apple 4 3) is not a tuple because its a list of atoms.
;
; '(3 (7 4) 13 9) because (7 4) is not a number.
'()
;
; A tuple is a list of numbers (non-negative integers in our case).

;;
;; Addtup
;;
;
; Definition:-
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))
(addtup '(3 5 2 8))     ;18
(addtup '(15 6 7 12 3)) ;43
;
; addtup builds a number by totalling all the numbers in its argument.
; Like cons building a list, we can use o+ to build numbers
; Like null? is a terminating condition for a list, it is also a terminating
; condition for a tuple. 0 is the result at termination like '() before.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The First Commandment (first revision)                     ;
;                                                            ;
; When recurring on a list of atoms, lat, ask two questions  ;
; about it: (null? lat) and else.                            ;
; When recurring on a number, n, ask two questions about it: ;
; (zero? n) and else.                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; x
;;
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))
(x 5 3)  ;15
(x 13 4) ;52

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fourth Commandment (first revision)                       ;
;                                                               ;
; Always change at least one argument while recurring. It must  ;
; be changed to be closer to termination. The changing argument ;
; must be tested in the termination condition:                  ;
; when using cdr, test termination with null? and               ;
; when using sub1, test termination with zero?.                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(x 12 3) ;36
; (x 12 3) => (o+ 12 (x 12 2)) => ... => (o+ 12 (o+ 12 (o+ 12 0))) => 36

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fifth Commandment                                                       ;
;                                                                             ;
; When building a value with +, always use 0 for the value of the             ;
; terminating line, for adding 0 does not change the value of an addition.    ;
;                                                                             ;
; When building a value with x, always use 1 for the value of the termination ;
; line, for multiplying by 1 does not change the value of a multiplication.   ;
;                                                                             ;
; When building a value with cons, always consider () for the value of the    ;
; terminating line.                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Tup+
;;
;
; Initial definition:-
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else (cons
             (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(3 6 9 11 4) '(8 5 2 0 7)) ;(11 11 11 11 11)
(tup+ '(2 3) '(4 6))              ;(6 9)
;
; tup+ takes two tuples of the same length and adds respective elements
; to produce a new tuple
(tup+ '(3 7) '(4 6)) ;(7 13)
;
; (tup+ '(3 7) '(4 6)) => (cons (o+ 3 7) (tup+ '(7) '(6))) => ... => '(7 13)
;
; (tup+ '(3 7) '(4 6 8 1)) is invalid bacause we expect tuples of the same length.
; We might want to modify tup+ to get '(7 13 8 1) in this case.
;
; New definition:-
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons
             (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(3 7) '(4 6 8 1)) ;(7 13 8 1)

;;
;; o>
;;
;
; Definition:-
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))
(o> 12 133) ;#f
(o> 120 11) ;#t

;;
;; o<
;;
; Definition:-
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))
(o< 4 6) ;#t
(o< 8 3) ;#f
(o< 6 6) ;#f

;;
;; o=
;;
;
; Definition:-
(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))
;
; eq? are for atoms and o= are for numbers

;;
;; ^
;;
;
; Definition:-
(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (^ n (sub1 m)))))))
(^ 1 1) ;1
(^ 2 3) ;8
(^ 5 3) ;125

;;
;; o/
;;
;
; Definition:-
(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))
(o/ 15 4) ;3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DON'T FORGET THE MUSTARD! ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Length
;;
;
; Definition:-
(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))
(len '(hotdogs with mustard sauerkraut and pickles)) ;6
(len '(ham and cheese on rye))                       ;5

;;
;; Pick
;;
;
; Definition:-
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(pick 4 '(lasagna spaghetti ravioli macaroni meatball)) ;macaroni
;
; (pick 0 '()) is invalid bacause we can't pick a 0th element
; pick takes a number and a lat. The number has to be smaller than
; the length of the lat. It returns the n-th item in list

;;
;; Rempick
;;
;
; Definition:-
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))
(rempick 3 '(hotdogs with hot mustard)) ;(hotdogs with mustard)

;;
;; Number?
;;
(number? 'tomato) ;#f
(number? 76)      ;#t
;
; number? is a primitive function

;;
;; No-nums
;;
;
; Definition:-
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
(no-nums '(5 pears 6 prunes 9 dates)) ;(pears prunes dates)
;
; no-nums removes all numbers from a lat

;;
;; All-nums
;;
;
; Definition:-
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))
(all-nums '(5 pears 6 prunes 9 dates)) ;(5 6 9)
;
; all-nums extracts numbers from alat and returns a tup

;;
;; Eqan?
;;
;
; Definition:-
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))
;
; eqan? can be used to check the equality of both atoms and numbers

;;
;; Occur
;;
;
; Definition:-
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a)
       (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))
;
; occur counts the number of times an atom appears in a lat

;;
;; One?
;;
;
; Definition:-
(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))
;
; one? can be used to replace (zero? (sub1 n)) calls in the previous functions
;
; Redefined rempick:-
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))