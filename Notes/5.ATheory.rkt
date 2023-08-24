#lang racket

;;;
;;; File:   RelatingToLambda.rkt
;;; Author: Your Glorious Instructor
;;; Purpose:
;;; Continue working through The Little Schemer, starting from Chapter 7

(require "cs514-useful.rkt")
(require rackunit)

;;; Into every computer scientist's life, some theory must fall.   Let's
;;; try to answer a problem we would like answered: Can I write a
;;; program that can check to see if my program halts for all input?
;;;

;;; Consider...

;;; The pick function returns the n-th element in a lat
;;;
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
        (pick (sub1 n) (cdr lat))))))


;;;
;;; Note what we mean by a `sorn`: symobl or number.
;;;
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat))))

;;; 
;;; The thing to consider: `keep-looking` is an example of an
;;; "unnatural" recursion as it does not recur on a part of `lat`.
;;; Problem is that while it does appear to get closer to its goal,
;;; there exists cased where it may not stop recurring.  For example,
;;; think what would happen if `lat` was a tuple (remember: list of
;;; numbers)? In mathematical logic and the theory of computation, this
;;; is called a "partial function"   The functions we've seen up to this
;;; point are "total functions".

;;; Now consider the most obvious partial function:

(define eternity
  (lambda (x)
    (eternity x)))

;;; Now let's build a function that will convert `'((a b) c)` into
;;; `'(a (b c)).

; The function shift takes a pair whose first component is a pair 
; and builds a pair by shifting the second part of the first component
; into the second component
;
(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair))
        (second pair)))))

; Example of shift
;
(check-equal? (shift '((a b) c))  '(a (b c)))
(check-equal? (shift '((a b) (c d)))  '(a (b (c d))))

;;; What is `shift` doing? It takes a pair whose first component is a
;;; pair and build a pair by shifting the second part of the first
;;; component into the second component.

;;; Now consider the following function:
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

;;; Both `align` and `shift` change their arguments for their recursive
;;; uses but in neither case is the change guaranteed to move us closer
;;; to the recursive base case.
;;;
;;; For example, the second form in the `cond` in `align` creates an
;;; argument for `align` that is not part of the original argument.
;;; Both the result and the argument of `shift` have the same number of
;;; atoms (Why?).

;;; OK, let's write some helper functions that we can try to use to
;;; moderate this problem:

;;; Counts the number of atoms in align's arguments
;;;
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (length* (first pora))
           (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (* (weight* (first pora)) 2)
           (weight* (second pora)))))))

; Example of weight*
;
(check-equal? (weight* '((a b) c))  7)
(check-equal? (weight* '(a (b c))  5)


;;; Playing around with `weight` gives us a good feel that `align` is
;;; total, which is good.

;;;
;;; Let's take a slightly different approach
;
(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else
        (build (first pora)
          (shuffle (second pora)))))))

; Example of shuffle
;
(shuffle '(a (b c)))                          ; '(a (b c))
(shuffle '(a b))                              ; '(a b)
; But the following will put the REPL into a infinite loop
;(shuffle '((a b) (c d)))                      

;;;
;;; Let's consider two classic examples of questions about totality.  We
;;; have here implementations of the Collatz and Ackermann functions.
;;; It's unproven as whether the Collatz function is total.   The
;;; Ackermann function is total but quickly becomes intractable.
;;;
; The one? function is true when n=1
;
(define one?
  (lambda (n) (= n 1)))

; not total function
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
        (cond
          ((even? n) (C (/ n 2)))
          (else
            (C (add1 (* 3 n)))))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else
        (A (sub1 n)
           (A n (sub1 m)))))))

; Example of A
(A 1 0)                                       ; 2
(A 1 1)                                       ; 3
(A 2 2)                                       ; 7
               
;;;
;;; Well, back to the original problem... it is possible for us to write
;;; a function that tells us whether some function returns with a value
;;; for every argument?
;;;

;;; Begin with a simpler case: write a function that checks whether some
;;; function stops for just the empty list.   Take a look at the
;;; discussion in TLS about the design of the `will-stop?`,  and then
;;; consider the implications behind the `last-try` function.   This is
;;; a contradiction proof for both this problem (the Halting Problem)
;;; and Godel's Incompleteness Theorem (A theory can be either complete
;;; or consistent but not both.).    More on that when you have to take
;;; a course on Theory of Computation.


;;; We find ourselves in interesting mathematical logic territory.
;;;

; length0
;
(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1 (eternity (cdr l))))))

; length<=1
;
(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1
        ((lambda(l)
           (cond
             ((null? l) 0)
             (else
               (add1 (eternity (cdr l))))))
         (cdr l))))))

; All these programs contain a function that looks like length.
; Perhaps we should abstract out this function.

; rewrite length0
;
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

; rewrite length<=1
;
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

; make length
;
(lambda (mk-length)
  (mk-length eternity))

; rewrite length<=1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
         (add1
           ((mk-length eternity) (cdr l))))))))

; It's (length '(1 2 3 4 5))
;
(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
         (add1
           ((mk-length mk-length) (cdr l))))))))
 '(1 2 3 4 5))

; 5


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else
            (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

; move out length function
;
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

; Y
;
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))

; it is called the applicative-order Y combinator.
;
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))


;;; The Y combinator is a formula which lets you implement recursion in
;;; a situation where functions can't have names but can be passed
;;; around as arguments, used as return values, and defined within other
;;; functions. 

;;; It works by passing the function to itself as an argument, so it can
;;; call itself.   This is a key part of the underlying logic in the
;;; theory of "lambda calculus", which we use as an entry point into
;;; type theory in programming languages.






