;;
;; Atoms
;;
(quote atom)
'atom
'turkey
;
; String of characters beginning with a letter is an atom
1492
;
; String of digits is an atom
'u
;
; *abc$ is not an atom since it starts with a special character

;;
;; Lists
;;
'(atom turkey or)
;
; Atoms enclosed by parentheses is a list
;
; '(atom turkey) or' is not a list, its just two s-expressions
'((atom turkey) or)

;;
;; S-expressions
;;
'xyz
;
; All atoms are S-expressions
'(x y z)
'((x y) z)
;
; All lists are s-expressions
'(how are doing so far)
'(((how) are) ((you) (doing so)) far)
;
; All lists of S-expressions are S-expressions
'()
;
; A list is not an atom
'(() () () ())

;;
;; Car
;;
(car '(a b c))         ;a
(car '((a b c) x y z)) ;(a b c)
;
; (car 'hotdog) is not valid. You can't ask car of an atom
;
; (car '()) is not valid. You can't ask car of an empty list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Law of Car                                         ;
;                                                        ;
; The primitive car is defined only for non-empty lists. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(car '(((hotdogs)) (and) (pickle) relish)) ;((hotdogs))
(car (car '(((hotdogs)) (and) (pickle) relish))) ;(hotdogs)

;;
;; Cdr
;;
(cdr '(a b c))         ;(b c)
(cdr '((a b c) x y z)) ;(x y z)
(cdr '(hamburger))     ;()
(cdr '((x) t r))       ;(t r)
;
; (cdr 'hotdog) is not valid. You can't ask cdr of an atom
;
; (cdr '()) is not valid. You can't ask cdr of an empty list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Law of Cdr                                         ;
;                                                        ;
; The primitive cdr is defined only for non-empty lists. ;
; The cdr of any non-empty list is always another list.  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Car and Cdr
;;
(car (cdr '((b) (x y) ((c))))) ;(x y)
(cdr (cdr '((b) (x y) ((c))))) ;(((c)))
;
; (cdr (car '(a (b (c)) d))) is invalid because we end up with (cdr 'a)

;;
;; Cons
;;
(cons 'peanut '(butter and jelly))                 ;(peanut butter and jelly)
(cons '(banana and) '(peanut butter and jelly))    ;((banana and) peanut butter and jelly)
(cons '((help) this) '(is very ((hard) to learn))) ;(((help) this) is very ((hard) to hearn))
;
; Cons takes two arguments, an s-expression and a list
(cons '(a b (c)) '()) ;((a b (c)))
(cons 'a '())         ;(a)
;
; (cons '((a b c)) 'b) is invalid since the second argument is not a list
;
; (cons 'a 'b) is also invalid since the second argument is not a list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Law of Cons                            ;
;                                            ;
; The primitive cons takes two arguments.    ;
; The second argumet to cons must be a list. ;
; The result is a list.                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cons 'a (car '((b) c d))) ;(a b)
(cons 'a (cdr '((b) c d))) ;(a c d)

;;
;; Null
;;
;
; () is the null list
(null? (quote ())) ;#t
(null? '(a b c))   ;#f
;
; (null? 'spaghetti) is invalid. You can't ask null? of an atom
; In practice, null? returns false for evertyhing except ()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Law of Null?                               ;
;                                                ;
; The primitive null? is defined only for lists. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Atom
;;
;
; 'Harry is an atom because its a string of characters beginning with a letter
;
;Defintion:-
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'Harry)                        ;#t
(atom? '(Harry had a heap of apples)) ;#f
;
; atom? takes any s-expression as an argument

;;
;; Atom, Car and Cdr
;;
(atom? (car '(Harry had a heap of apples)))         ;#t
(atom? (cdr '(Harry had a heap of apples)))         ;#f
(atom? (cdr '(Harry)))                              ;#f
(atom? (car (cdr '(swing low sweet cheery oat))))   ;#t
(atom? (car (cdr '(swing (low sweet) cheery oat)))) ;#f

;;
;; Eq
;;
;
; 'Harry and 'Harry are the same atoms
(eq? 'Harry 'Harry)      ;#t
(eq? 'margarine 'butter) ;#f
;
; eq? takes two arguments. Both should be non-numeric atoms
;
; (eq? '() '(strawberry)) is invalid since the arguments are not atoms.
; In practice, lists can be used as arguments to check if they're the same list.
;
; (eq? 6 7) would be invalid since the arguments are numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Law of Eq?                         ;
;                                        ;
; The primitive eq? takes two arguments. ;
; Each must be a non-numeric atom.       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eq? (car '(Mary had a little lamb chop)) 'Mary)                                              ;#t
(eq? (cdr '(soured milk)) 'milk)                                                              ;#t
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans)))) ;#t

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; THIS SPACE RESERVED FOR ;
;      JELLY STAINS!      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;