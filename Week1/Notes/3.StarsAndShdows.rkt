#lang racket
;;; File:   StarsAndShadows.rkt
;;; Author: Your Glorious Instructor
;;; Purpose:
;;; Continue working through The Little Schemer, starting from Chapter 5

(require "cs514-useful.rkt")
(require rackunit)

;;; And now back to our program
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))
(define l '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
check-equal? (lat? l) #f)

;;;
;;; Note how this function implements the Fourth Commandment:
;;;
(define insertR*
  (lambda ( new old l)
    (cond
      ( ( null? l) (quote ()) )
      ( ( atom? ( car l))
        (cond
          (( eq? ( car l) old )
           ( cons old
                  ( cons new
                         ( insertR* new old
                                    (cdr l)))))
          (else (cons (car l)
                      (insertR* new old
                                (cdr l))))))
      (else (cons (insertR* new old
                            ( car l))
                  (insertR* new old
                            (cdr l)))))))

;;; We can now state the final super-duper version of The Fourth Commandment:
;;; - If recurring upon a list of atoms, `lat`, ask two questions about
;;; it: `(null? lat) and else.
;;; - If recurring upon a number `n`, ask two questions about it:
;;; `(zero? n) and else.
;;; - If recurring upon a list of S-expressions `l`, ask three questions
;;; about it: `(null? l)`, `(atom? (car l))`, and else.
;;;

;;; Notice the use of the * in names.  "Star" functions recurs with both
;;; the `car` and `cdr` of a list.  The recursion happens when it finds
;;; out that the car is a list
;;;
(define a 'banana)
(define aList '((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy)))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
       (else (+ (occur* a (car l))
                (occur* a (cdr l))
                )))))

(check-equal? (occur* a aList) 5)

;;;
;;; And we can firmly specify The Fourth Commandment:
;;; Always change at least one argument while recurring.
;;; - If a list of atoms, use the `cdr` of the list.
;;; - On a number, use `(sub` n)`.
;;; - On a list of S-expressions, use `(car l)` and `(cdr l)` if neither
;;; `(null? l) nor `(atom? (car l))` are true.
;;;
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
       (else
        (cons (subst* new old (car l))
              (subst* new old (cdr l)))))))

;;;
;;; It's all a matter of asking a question on each conditional
;;;
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? 2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2)))))))

(check-equal? (eqlist? '(1 2 3) '(1 2 3)) (eqlist2? '(1 2 3) '(1 2 3)))
;;;
;;; So, let's rewrite the remove member function to use S-expressions
;;; The function should now remove the first matching S-exp s in l, instead of
;;; the first matching atom a in lat
;;;
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l)
                          (rember s (cdr l)))))))))

;;;
;;; Now, on to Chapter 6, and considering questions about equality and reality
;;;
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote *))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^))
       (and (numbered? (car aexp))
            (numbered? (car (cdr aexp))))))))

;;;
;;; Things get interestingly funky at this point and you should follow
;;; along with the exposition in the book.   Take away: representation
;;; and shadowing.
