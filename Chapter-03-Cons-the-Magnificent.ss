(load "Chapter-02-Do-It-Do-It-Again-And-Again-And-Again.ss")

;;
;; Rember
;;
;
; Definition:-
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))
(rember 'mint '(lamb chops and mint jelly))               ;(lamb chops and jelly)
(rember 'mint '(lamb chops and mint flavored mint jelly)) ;(lamb chops and flavored mint jelly)
(rember 'toast '(bacon lettuce and tomato))               ;(bacon lettuce and tomato)
(rember 'cup '(coffee cup tea cup and hick cup))          ;(coffee tea cup and hick cup)
;
; rember takes an atom and a lat as its arguments.
; It makes a new lat with first occurrence of the atom in old lat removed.
(rember 'bacon '(bacon lettuce and tomato)) ;(lettuce and tomato)
;
; goes through (eq? 'bacon 'bacon) => (lettuce and tomato)
(rember 'and '(bacon lettuce and tomato)) ;(bacon lettuce tomato)
;
; goes through (eq? 'and 'bacon) => (cons 'bacon (rember 'and (lettuce and tomato))) => ...
; => (eq? 'and 'and) => (tomato) => (cons bacon (cons lettuce (tomato))) => (bacon lettuce tomato)
(rember 'sauce '(soy sauce and tomato sauce)) ;(soy and tomato sauce)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Second Commandment   ;
;                          ;
; Use cons to build lists. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Firsts
;;
;
; Definition
(define firsts
  (lambda (l)
    (if (null? l)
        '()
        (cons (caar l)
              (firsts (cdr l))))))
(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant))) ;(apple plum grape bean)
(firsts '((a b) (c d) (e f)))      ;(a c e)
(firsts '())                       ;()
(firsts '((five plums)
          (four)
          (eleven green oranges))) ;(five four eleven)
(firsts '(((five plums) four)
          (eleven green oranges)
          ((no) more)))            ;((five plums) eleven (no))
;
; firsts takes one argument, an empty list or a list of non-empty lists.
; It builds a list with the first element of each of the lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Third Commandment                                     ;
;                                                           ;
; When building a list, describe the first typical element, ;
; and then cons it onto the natural recursion.              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; In firsts above, a typical element is (caar l)

;;
;; InsertR
;;
;
; Definition:-
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old
                                 (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old (cdr lat)))))))
(insertR 'topping 'fudge '(ice cream with fudge for dessert)) ;(ice cream with fudge topping for dessert)
(insertR 'jalapeno 'and '(tacos tamales and salsa))           ;(tacos tamapes and jalapeno salsa)
(insertR 'e 'd '(a b c d f g d h))                            ;(a b c d e f g d h)
;
; insertR takes three arguments: two atoms new and old, and a lat. The function builds a new list
; with the new atom inserted to the right of the first occurrence of the old atom

;;
;; InsertL
;;
;
; Definition:-
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat)
                  (insertL new old (cdr lat)))))))
;
; insertL is similar to insertR, except it inserts the new atom to the left instead of right

;;
;; Subst
;;
;
; Definition:-
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old (cdr lat)))))))
;
; subst replaces the first occurrence of old with new
(subst 'topping 'fudge '(ice cream with fudge for dessert)) ;(ice cream with topping for dessert)

;;
;; Subst2
;;
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 (new o1 o2 (cdr lat))))))))
;
; subst2 takes two old atoms instead and replaces the first occurrence of either atoms with the new
(subst2 'vanilla 'chocolate 'banana
        '(banana ice cream with chocolate topping)) ;(vanilla ice cream with chocolate topping)

;;
;; Multirember
;;
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))
        
;
; multirember is similar to rember but it removes all occurrences of the atom as opposed to just the first one
(multirember 'cup '(coffee cup tea cup and hick cup)) ;(coffee tea and hick)
;
; goes through (eq? 'cup 'cup) => (multirember 'cup '(tea cup and hick cup)) => ... => '(tea and hick)

;;
;; MultiinsertR
;;
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old
                                 (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))

;;
;; MultiinsertL
;;
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new
                                 (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old (cdr lat)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fourth Commandment (preliminary)                                       ;
;                                                                            ;
; Always change at least one argument while recurring. It must be changed to ;
; be closer to termination. The changing argument must be tested in the      ;
; termination condition: when using cdr, test termination with null?         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Multisubst
;;
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old (cdr lat)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONS A CAKE ONTO YOUR MOUTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;