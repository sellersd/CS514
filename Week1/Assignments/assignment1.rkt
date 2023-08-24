#lang racket


(require "cs514-useful.rkt")
(require rackunit)

;;; Create 10 lats given 5 atoms
'(ant bee cat dog eel)
'(ant)
'(bee)
'(cat)
'(ant bee)
'(ant cat)
'(ant dog)
'(ant bee cat)
'(ant cat dog)
'(ant bee cat dog)
'(ant cat dog eel)

(displayln "Problem 2")
;;; Create lists using cons

(define a 'all)
(define b 'these)
(define c 'problems)

;;; Goals (all (these problems))
(check-equal? (cons a (cons (cons b (cons c '())) '())) '(all (these problems)))


;;; Goals (all (these) problems)
(check-equal? (cons a (cons (cons b '()) (cons c  '()))) '(all (these) problems))

;;; Goals ((all these) problems)
(check-equal? (cons (cons a  (cons b '())) (cons c  '())) '((all these) problems))

;;; Goals ((all) these problems)
(check-equal? (cons (cons a '()) (cons b  (cons c  '()))) '((all) these problems))


(displayln "Problem 3")
;;; (car (cons a l), a=french, l=(fries)
(define a1 'french)
(define l '(fries))
(check-equal? (car (cons a1 l)) 'french)



(displayln "Problem 4")
;;; Can (null? (cons a2 l)) #t
(define a2 '())
(null? (cons a2 '()))
;;;No, let l be the empty list, then (cons a l) results in the non-empty list (a). All other list contain more elements than the empty list, thus they will still have more elements than the empty list after application of cons.


(displayln "Problem 5")
(null? (cdr '((meatballs))))

(displayln "Problem 6")
(set! l '(a b c d e f g))
(set! a 'p)
;;; (set! l '())
;;; (set! a 'a)
(define t #t)

(cond 
  ((null? l) '())
  (t (or
       (eq? (car l) a )
       (member? a (cdr l)))))

(if 
     (null? l)
     (quote ())
     (or
       (eq? (car l) a) (member? a (cdr l))))

;;; 7
(display "Problem 7\n")
(set! l '((a b c) (d e) (f g)))
(define m '((a b c) d e (f g)))
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define nonlat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) #f)
      (else (nonlat? (cdr l))))))

(check-equal? (nonlat? l) #t "fail")
(check-equal? (nonlat? m) #f "fail")


(display "Problem 8\n")
(define member-twice?
  (lambda (a l)
    (cond 
      ((null? l) '())
       )))


(display "Problem 9\n")
(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
       (else (cons (car (cdr (car l))) (seconds (cdr l))))
       )))

(check-equal? (seconds '((a b c) (e f g) (h i j) (k l m))) '(b f i l) #f)
