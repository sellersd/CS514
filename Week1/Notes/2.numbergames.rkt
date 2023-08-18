#lang racket
;;;
;;; File:   numbergames.rkt
;;; Author: Your Glorious Instructor
;;; Purpose:
;;; Take the code we built in the basics of Racket and use it to build a
;;; Racket package that we can use in other cases.
;;;

;;; # A little theory comes into all of lives at times
;;;
;;; In Chapter 4, TLS does a dive into using Scheme (Racket, in our
;;; case) to do some Theory of Computation stuff.   It's important, as
;;; it is going to be our pathway into the upcoming discussion on type
;;; theory and denotation semantics.
;;;
;;; The thought process in this chapter is to show how we can specify
;;; a logical system using a functional programming language, which in
;;; this case is Peano's rules of arithmetic.  In an upcoming chapter,
;;; we take this and Racket's implementation of lambda calculus to show
;;; that one cannot write a computer program that determines if a
;;; program will successfully terminate (or as we say in Theory of
;;; Computation, show that the Halting Problem is undecidable).

;;; Consider how we might go about building a set of functions to do
;;; arithmetic on the non-negative integers.
(require rackunit)

(define add1
  (lambda (n)
    (+ n 1) ))

(define sub1
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (- n 1)))))

(begin
  (pretty-print "add1 to 32")
  (check-equal? (add1 32) 33)
  (pretty-print "sub1 from 33")
  (check-equal? (sub1 33) 32)
  (pretty-print "Sub1 from 0")
  (check-equal? (sub1 0) 0)
)

;;; We're begin somewhat meta here as we're using the system operations
;;; to do this.   Think of it working with assembly language (of sorts)
;;; We also assume the existence of a predicate `zero?` that does what
;;; you would expect with non-negative integers.

;;; Let's try our hand at addition.  The symbol "o+" will represent that
;;; weird typographic thing in TLS.

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

;;; Subtraction is similar
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(begin
  (pretty-print "4 + 3")
  (o+ 4 3)
  (pretty-print "4 - 3")
  (o- 4 3)
  (pretty-print "4 + 0")
  (o+ 4 0)
  (pretty-print "4 - 0")
  (o- 4 0)
  )

;;; Now consider the concept of a tuple, which is a list of numbers. For
;;; example '(1 2 3 9 72) is a tuple while '(1 2 3 apple 4 3) and
;;; '(3 (7 4) 13 9) are not (Can you explain why this is so?).

;;; Now let us write a function that will sum the contents of a tuple.
;;; Observe how similar this is to `rember` function from earlier.
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

;;; And notice how we keep using natural recursion
(define ox
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (ox n (sub1 m)))))))

(check-equal? (ox 12 3) 36)

;;; A few notes:
;;; 1. See how numbers follow the same pattern as atoms and list when
;;; thinking recursively.
;;; 2. And use the correct identity with 0 for o+, 1 for 0x, and () for
;;; cons.

;;; Let's try a tup-o+ ...
(define tup-o+-bad
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote ()))
      (else
       (cons (o+ (car tup1) (car tup2))
             (tup-o+-bad (cdr tup1) (cdr tup2)))))))

(check-equal? (tup-o+-bad '(3 7) '(4 6)) '(7 13))

;;; What's bad about our first attempt?  What happens if the tuples are
;;; not the same length!

;;; Here's better ...
(define tup-o+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2))
             (tup-o+ (cdr tup1) (cdr tup2)))))))

;;; And now relational operators!
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #t)
      ((zero? n) #f)
      (else (o> (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

;;; Now follow along in TLS showing the different ways we use these
;;; functions to pull things out of lats and other uses of our
;;; math prowess.


;;; # The Moral of the Story
;;;
;;; We have done here the steps that mathematical logicians took in late
;;; 19th century to formalize the rules of arithmetic, which lead to a
;;; science of number theory in math and eventually to people like
;;; Godel, Church, and Turing formalizing (in the math sense) the rules
;;; that are theoretical basis of computer science.
;;;




