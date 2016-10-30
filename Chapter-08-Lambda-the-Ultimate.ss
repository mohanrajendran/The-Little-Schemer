(load "Chapter-07-Friends-and-Relations.ss")

;;
;; Rember-f
;;
;
; Definition:-
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember-f test? a (cdr l)))))))
(rember-f = 5 '(6 2 5 3))                                       ;(6 2 3)
(rember-f eq? 'jelly '(jelly beans are good))                   ;(beans are good)
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))) ;(lemonade and (cake))

;;
;; Currying
;;
;
; (lambda (a l) ... ) is a function of two arguments, a and l
;
; (lambda (a)
;   (lambda (x)
;     (eq? x a))) is a function that takes an argument a and returns a function
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
;
; We can use the above to generate comparison function for any atom we can want
(define eq?-salad (eq?-c 'salad))
(eq?-salad 'salad) ;#t
(eq?-salad 'tuna)  ;#f
;
; We need not give a name to eq?-salad
((eq?-c 'salad) 'tuna) ;#f

;;
;; Curried rember-f
;;
;
; We can rewrite rember-f as a function that takes test? alone and returns a rember-like
; function with eq? replaced by test?
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))
(rember-eq? 'tuna '(tuna salad is good))              ;(salad is good)
(rember-eq? 'tuna '(shirmp salad and tuna salad))     ;(shrimp salad and salad)
(rember-eq? 'eq? '(equal? eq? eqan? eqlist? eqpair?)) ;(equal? eqan? eqlist? eqpair?)

;;
;; InsertL-f
;;
;
; Definition:-
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

;;
;; InsertR-f
;;
;
; Defintion:-
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old (cdr l))))))))

;;
;; Insert-g
;;
;
; Looking at insertR and insertL, we can see that they only differ in one line.
; We can now create a function that would allow us to choose one or the other.
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))
;
; Now we can simply redefine insertL and insertR
(define insertL (insert-g seqL))
(define insertR (insert-g seqR))
;
; We might not even need to create names functions seqL or seqR. This might be better if
; we don't want to remember as many words
(define insertL (insert-g (lambda (new old l)
                            (cons old (cons new l)))))
;
; We can even re-define subst
(define seqS
  (lambda (new old l)
    (cons new l)))
(define subst (insert-g seqS))
;
; Also re-define rember.
; Note the use of #f, this is because the function produced by insert-g expects three arguments.
; But rember only give two arguments. #f is simply a place-holder for an unused variable.
(define seqrem
  (lambda (new old l)
    l))
(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
(rember 'sausage '(pizza with sausage and bacon)) ;(pizza with and bacon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Ninth Commandment                         ;
;                                               ;
; Abstract common patterns with a new function. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Value re-defined
;;
;
; We can re-define value by abstracting away the common pattern
(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) o+)
      ((eq? x '*) x)
      (else ^))))
(atom-to-function (operator '(+ 5 3))) ;#<procedure:o+>

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function
         (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))

;;
;; Multirember re-defined
;;
;
; We can redefine multirember to use our test function:-
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)) ;(shrimp salad salad and)
(define multirember-eq? (multirember-f eq?))

;
; We can instead combine test and a and produce another version:-
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test?
                                (cdr lat)))))))
(define eq?-tuna (eq?-c 'tuna))
(multiremberT eq?-tuna '(shrimp salad tuna salad tuna)) ;(shrimp salad salad and)

;
; Let us take a complicated function like below:-
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))
;
; let us try to decipher it with the function below:-
(define a-friend
  (lambda (x y)
    (null? y)))
(multirember&co 'tuna '() a-friend)     ;#t
(multirember&co 'tuna '(tuna) a-friend) ;#f
;
; (multirember&co 'tuna '(tuna) a-friend) => (multirember&co 'tuna '() (lambda (newlat seen) (a-friend newlat (cons 'tuna seen))))
; => (a-friend '() (cons 'tuna '()) => (null? (cons 'tuna '())) => #f
(multirember&co 'tuna '(and tuna) a-friend) ;#f
;
; (multirember&co 'tuna '(and tuna) a-friend) => (multirember&co 'tuna '(tuna) (lambda (newlat seen) (a-friend (cons 'and newlat) seen)))
; => (multirember&co 'tuna '() (lambda (newlat seen) (a-friend (cons 'and newlat) (cons 'tuna seen))))
; => (a-friend (cons 'and '()) (cons 'tuna '())) => #f
;
; With this, we can finally see that multrember&co collects the lat argument elements into two lists. One which is the same as argument a
; and the rest. Then it applies col on the collected list. In the above case, we check if the given element is present in the list.
(multirember&co 'tuna
                '(strawberries tuna and swordfish)
                (lambda (x y)
                  (length x)))                     ;3
;
; The above function simply counts the number of elements not equal to the given one

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Tenth Commandment                                     ;
;                                                           ;
; Build functions to collect more than one value at a time. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; MultiinsertLR
;;
;
; Definition:-
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))
;
; we can write a multiinsertLR&co as well
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (add1 L)
                                R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L
                                (add1 R)))))
      (else
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat)
                                L
                                R)))))))
;
; We can see that multiinsertLR&co does the work on multiinsertLR and counts the number of times
; oldL and oldR was found and provides them to col to calculate whatever it needs
; (multiinsertLR&co 'salty
;                   'fish
;                   'chips
;                   '(chips and fish or fish and chips))
; => (col '(chips salty and salty fish or salty fish and chips salty) 2 2)

;;
;; Evens-only*
;;
;
; Definition:-
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) ;((2 8) 10 (() 6) 2)

;
; Let us write evans-only*&co that collects the list without odd numbers, product of even numbers and sum of odd numbers
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (x (car l) p)
                                 s))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p
                                 (o+ (car l) s)))))))
      (else (evens-only*&co (car l)
                            (lambda (al ap as)
                              (evens-only*&co (cdr l)
                                              (lambda (dl dp ds)
                                                (col (cons al dl)
                                                     (x ap dp)
                                                     (o+ as ds))))))))))
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                the-last-friend)                ;(38 1920 (2 8) 10 (() 6) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GO EAT A PRETZEL; DON'T FORGET THE MUSTARD. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;