(load "Chapter-04-Numbers-Games.ss")

;;
;; Rember*
;;
;
; Definition:-
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))
(rember* 'cup '((coffee)
                cup
                ((tea) cup)
                (and (hick))
                cup))                      ;((coffee)((tea)) (and (hick)))
(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce))) ;(((tomato)) ((bean)) (and ((flying))))

;;
;; Lat revisited
;;
(lat? '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))) ;#f
;
; This is false because the some elements of the argument list are lists

;;
;; InsertR*
;;
;
; Definition:-
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old
                                  (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))
(insertR* 'roast
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))
;((how much (wood))
; could
; ((a (wood) chuck roast))
; (((chuck roast)))
; (if (a) ((wood chuck roast)))
; could chuck roast wood)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The First Commandment (final version)                                ;
;                                                                      ;
; When recurring on a list of atoms, lat, ask two questions about it:  ;
; (null? lat) and else.                                                ;
;                                                                      ;
; When recurring on a number, n, ask two questions about it: (zero? n) ;
; and else.                                                            ;
;                                                                      ;
; When recurring on a list of S-expressions, l, ask three questions    ;
; about it: (null? l), (atom? (car l)), and else.                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; rember* and insertR* are similar in that they follow the final part of
; the first commandment. They differ from the multi- version in that
; they also recur on the car part if its a list. Both versions recur on
; the cdr. This is because * functions work on list of lists too.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fourth Commandment (final version)                                  ;
;                                                                         ;
; Always change at least one argument while recurring. When recurring on  ;
; a list of atoms, lat, use (cdr lat). When recurring on a number, n, use ;
; (sub1 n). And when redurring on a list of S-expressions, l, use (car l) ;
; and (cdr l) if neither (null? l) nor (atom? (car l)) are true.          ;
;                                                                         ;
; It must be changed to be closer to termination. The changing argument   ;
; must be tested in the termination condition:                            ;
;                                                                         ;
; when using cdr, test termination with null? and                         ;
; when using sub1, test termination with zero?                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Occur*
;;
;
; Definition:-
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))
(occur* 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet))
                  (banana)
                  (bread)
                  (banana brandy)))         ;5

;;
;; Subst*
;;
;
; Definition:-
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))
(subst* 'orange
        'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))
;((orange)
; (split ((((orange ice)))
;         (cream (orange))
;         sherbet))
; (orange)
; (bread)
; (orange brandy))

;;
;; InsertL*
;;
;
; Definition:-
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new
                                  (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))
(insertL* 'roast
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))
;((how much (wood))
; could
; ((a (wood) roast chuck))
; (((roast chuck)))
; (if (a) ((wood roast chuck)))
; could roast chuck wood)

;;
;; Member*
;;
;
; Definition:-
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))
(member* 'chips '((potato) (chips ((with) fish) (chips)))) ;#t
;
; This method returns #t based on the first instance it finds

;;
;; Leftmost
;;
;
; Defintion:-
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))  
(leftmost '((potato) (chips ((with) fish) (chips)))) ;potato
(leftmost '(((hot) (tuna (and))) cheese))            ;hot
;
; (leftmost '(((() four)) 17 seventeen))) has no answer since there is no leftmost element
; (leftmost '()) has no answer as well since we don't check for null
;
; leftmost finds the leftmost atom in a non-empty list of S-expressions that does not
; contain the empty list.

;;
;; And Or
;;
(and (atom? (car '(mozzarella pizza)))
     (eq? (car '(mozzarella pizza)) 'pizza))            ;#f
(and (atom? (car '((mozzarella mushroom) pizza)))
     (eq? (car '((mozzarella mushroom) pizza)) 'pizza)) ;#f
;
; and asks questions one at a time until it finds one whose value is false. Then, it stops with false.
; If none of the expressions are false, (and ...) is true
;
; or asks questions one at a time until it finds one that is true. Then (or...) stops with true.
; If none of the expressions are true, (or ...) is false
;
; Since cond is similar in the sense that it does not evaluate all its expressions always, we can rewrite
; 'and' and 'or' with cond
; (and a b) = (cond (a b) (else #f))
; (or a b)  = (cond (a #t) (else b))

;;
;; Eqlist?
;;
;
; Definition:-
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))
(eqlist? '(strawberry ice cream) '(strawberry ice cream))                  ;#t
(eqlist? '(strawberry ice cream) '(strawberry cream ice))                  ;#f
(eqlist? '(banana ((split))) '((banana) (split)))                          ;#f
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) ;#f
;
; eqlist? determines if two lists are equal

;;
;; Eqlist? simplification
;;
;
; We can simplify null checks as follows:-
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1))
           (atom? (car l2)))
       #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))
;
; Recall that an S-expression is either an atom or a list of S-expressions
; To check if two s-expressions are equal:-
(define +equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; Rewriting eqlist? using equal?
(define  eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (+equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Sixth Commandment                        ;
;                                              ;
; Simplify only after the function is correct. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Rember revisited
;;
;
; We can rewrite rember to remove and s-expression from a list of s-expressions
;
; First we add check for atoms in the old lat:-
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((equal? (car l) s) (cdr l))
         (else (cons (car l)
                  (rember s (cdr l))))))
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l)
                          (rember s
                                  (cdr l)))))))))
;
; Next we squash atom? and else branches
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l)
                          (rember s (cdr l)))))))))
;
; Next we combine inner cond with the outer one
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))
