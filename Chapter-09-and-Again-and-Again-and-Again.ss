(load "Chapter-08-Lambda-the-Ultimate.ss")

;;
;; Looking
;;
;
; Definition:-
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))
;
; sorn stands for symbol or number
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
;
; keep-looking does not recur on part of lat. Thus, if we try with lat
; as (7 2 4 7 5 6 3), we will not stop looking.
; looking is called a partial function as opposed to total functions.
; This means that it does not reach its goal for some of its arguments
(looking 'caviar '(6 2 4 caviar 5 7 3))     ;#t
(looking 'caviar '(6 2 grits caviar 5 7 3)) ;#f
;
; Another example of a partial function is the eternity function:-
(define eternity
  (lambda (n)
    (eternity n)))
;
; It never yield for any argument.

;;
;; Shift
;;
;
; Definition:-
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second  (first pair))
                  (second pair)))))                           
(shift '((a b) c))     ;(a (b c))
(shift '((a b) (c d))) ;(a (b (c d)))
;
; shift takes a pair whose first component is a pair and builds a pair
; by shifting the second part of the first component into the second component.

;;
;; Align
;;
;
; Definition:-
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))
;
; We are not guaranteed that align makes progress because the second cond-line
; calls shift which produces an argument that is not part of the original argument.
; Thus, this could be a partial function. Let us define functions to measure the
; progress and verify it.

;;
;; Length*
;;
;
; Definition:-
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (o+ (length* (first pora))
           (length* (second pora)))))))
;
; Length* is not a suitable function to measure align's progress.
; First component of a pair becomes simpler while the second component becomes more complex.
; They do so at the same rate. So length remains constant as align proceeds.

;;
;; Weight*
;;
;
; Definition:-
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (o+ (x (weight* (first pora)) 2)
           (weight* (second pora)))))))
(weight* '((a b) c)) ;7
(weight* '(a (b c))) ;5
;
; This wwight decreases as align proceeds. And weight can never be negative. This means that
; align produces a value for every argument. align is a total function.

;;
;; Shuffle
;;
;
; Definition:-
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))
;
; Again, we don't know yet if shuffle is a total function.
(shuffle '(a (b c)))     ;(a (b c))
(shuffle '(a b))         ;(a b)
;
; (shuffle '((a b) (c d))) does not yeild a value.
; (shuffle '((a b) (c d))) => (shuffle '((c d) (a b))) => (shuffle '((a b) (c d))) => ...
; We keep recurring on the same value. So this does not yield a new value.

;;
;; Collatz function
;;
;
; Definition:-
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (o/ n 2)))
      (else (C (add1 (x 3 n)))))))

;;
;; Ackermann function
;;
;
; Definition:-
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

;;
;; Haltability
;;
;
; Let us attempt to write a function that determines if some function would stop for and empty list
; (define will-stop?
;  (lambda (f)
;    ...))
; It returns #t if it halts and #f if it doesn't.
; Lets try with some functions:-
;
; (will-stop? length) should be #t since (length '()) is 0.
; (will-stop? eternity) should be #f.
;
; Let us try this function:-
(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))
;
; (last-try '()) => (and (will-stop? last-try) (eternity '()))
;
; If (will-stop? last-try) is #f, we get (last-try '()) to be #f due to short-circuting and.
; But since will-stop? halts, (will-stop? last-try) should be #t, which goes contrary to what we have defined.
;
; But if (will-stop? last-try) #t, we get return (eternity '()) which does not halt.
; This implies (will-stop? last-try) to be #f.
;
; This means that if we can define will-stop?, it should halt with #t or #f. In the above case, it cannot.
; Thus, will-stop? cannot be defined. will-stop? is a function that we can describe but cannot define as a total function.

;;
;; Y-combinator
;;
;
; define is a keyword used to define named functions

; The function below:-
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))
;
; can be used to find lengths of empty lists. Lets call it length_0
;
; Now lets try to define a function that finds the length of lists with one or fewer items:-
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
           ((lambda (l)
              (cond
                ((null? l) 0)
                (else (add1
                       (eternity (cdr l))))))
            (cdr l))))))
;
; We can define a function, length_2, for lists with two or fewer items similarly:-
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
           ((lambda (l)
              (cond
                ((null? l) 0)
                (else (add1
                       ((lambda (l)
                          (cond
                            ((null? l) 0)
                            (else (add1
                                   (eternity (cdr l))))))
                        (cdr l))))))
            (cdr l))))))
;
; By repeating it to infinity, we can write a function that can determine the length of any list.
; But we can't write an infinite function. We can abstract out the recurrence function and
; use it as an argument.
;
; Let us re-create length_0:-
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)
;
; Similarly, length_1:-
((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))
;
; Next, length_2:-
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)))
;
; We can see the recurrence relation happening. We take a function and return a new function
; that takes a list and returns 0 if null? else add1 to the argument function applied on the cdr of list
; Now, we can name this function that takes length and returns a new function as mk-length.
; This helps abstract that pattern. Our base case applies it to eternity.
;
; length_0 redefined:-
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;
; length_1 redefined:-
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;
; length_2 redefined:-
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;
; We still have the same problem, we need to call mk-length as many times as the function we need.
; Let us replace eternity by that mk-length instead. And use mk-length instead of length.
;
; length_0:-
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))
;
; length_1:-
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity)
                    (cdr l))))))))
;
; Interlude, lets apply length_1 on (apples)
(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity)
                    (cdr l))))))))
 '(apples))                              ;1
;
; Let's trace the evaluation. Firstly, (mk-length mk-length) binds '(apples) to l and calls the inner function:-
; (add1 ((mk-length eternity) 1())) => (null? '()) => 1+0 => 1. Any list longer than size 1 would hit the
; eternity and never end evaluating.
;
; To prevent eternity from being called, let us call mk-length again like we did in the second line of the above function:-
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length)
                    (cdr l))))))))
;
; This works by recursively constructing length function for longer lists as required. This is because cond clauses are
; evaluated lazily.
;
; Now lets abstract out the inner (mk-length mk-length)
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))
;
; This obviously will not work, because the inner (mk-length mk-length) is always eagerly evaluated. Thus, it will never end.
;
; Let us see how to fix this by wrapping up the evaluation in a function:-
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))
;
; With one more extraction, we get this:-
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;
; With this transformation, we get a function that converts a function that looks like length and converts it to length.
; That function is the Y-combinator:-
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))