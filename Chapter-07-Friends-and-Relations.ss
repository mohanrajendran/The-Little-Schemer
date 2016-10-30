(load "Chapter-06-Shadows.ss")

;;
;; Sets
;;
;
;'(apple peaches apple plum) is not a set since apple appears more than once
'(apples peaches pear plums)
'()

;;
;; Set?
;;
;
; Definition using member? from before:-
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))
(set? '(apple 3 pear 4 9 apple 3 4)) ;#f

;;
;; Makeset
;;
;
; Definition:-
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))
(makeset '(apple peach pear peach plum apple lemon peach)) ;(pear plum apple lemon peach)
;
; Re-define makeset using multirember:-
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset (multirember (car lat)
                                        (cdr lat))))))))
(makeset '(apple peach pear peach plum apple lemon peach)) ;(apple peach pear plum lemon)
;
; The two definitions of makeset produce different ordering because of how they work:-
; - First one does not include first atom if they are present in the list
; - Second one removes instance of first atom from the rest of the list

;;
;; Subset?
;;
;
; Definition:-
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))
(subset? '(5 chicken wings)
         '(5 hamburgers 2 pieces fried chicken and light duckling wings)) ;#t
(subset? '(4 pounds of horseradish)
         '(four pounds chicken and 5 ounces horseradish))                 ;#f
;
; subset? takes two lats and determines if the first lat is a subset of the second

;;
;; Eqset?
;;
;
; Definition:-
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))
;
; eqset? takes two lat and determines if they are equal sets

;;
;; Intersect?
;;
;
; Definition:-
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))))))
(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))         ;#t
;
; intersect? takes two lat and determines if they have common elements

;;
;; Intersect
;;
;
; Definition:-
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))         ;(and macaroni)

;;
;; Union
;;
;
; Definition:-
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))
(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))                   ;(stewed tomatoes casserole macaroni and cheese)

;;
;; Difference
;;
;
; Definition:-
(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1)
                  (difference (cdr set1) set2))))))
;
; difference takes two lats and returns a new lat which contains elements in lat1 that are not in lat2

;;
;; Intersectall
;;
;
; Definition:-
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))
(intersectall '((a b c)
                (c a d e)
                (e f g h a b)))                   ;(a)
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples))) ;(6 and)

;;
;; Pairs
;;
'(3 7)
'((2) (pair))
;
; a pair is a list with two s-expressions

;;
;; A-pair?
;;
;
; Definition:-
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
(a-pair? '(full (house))) ;#t

;
; pairs can be manipulated using helper functions
(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;;
;; Rel
;;
;
; '(apples peaches and pumpkin pie) is not a relation since it is not a list of pairs
;
; '((apples peaches) (pumpkin pie) (applies peaches)) is not a relation since it is not a set of pairs
'((apples peaches) (pumpkin pie))
'((4 3) (4 2) (7 6) (6 2) (3 4))

;;
;; Fun
;;
;
; '((4 3) (4 2) (7 6) (6 2) (3 4)) is not a function since these are two pairs with first element of 4.
'((8 3) (4 2) (7 6) (6 2) (3 4))
;
; '((d 4) (b 0) (b 9) (e 5) (g 4)) is not a function since b is repeasted

;;
;; Fun?
;;
;
; Definition:-
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;;
;; Revrel
;;
;
; Definition:-
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel))
                         (first (car rel)))
                  (revrel (cdr rel)))))))
(revrel '((8 a) (pumpkin pie) (got sick))) ;((a 8) (pie pumpkin) (sick got))
;
; By abstracting the revesal with revpair, we can rewrite revrel:-
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

;;
;; Fullfun
;;
;
; '((8 3) (4 2) (7 6) (6 2) (3 4)) is not a fullfun since 2 appears more than once
; in the second item of a pair
'((8 3) (4 8) (7 6) (6 2) (3 4))
;
; '((grape raisin) (plum prune) (stewed prune)) is not a fullfun.
'((grape raisin) (plum prune) (stewed grape))

;;
;; Fullfun?
;;
;
; Definition:-
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
;
; another way to call fullfun? is one-to-one?. It can be defined as follows:-
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))