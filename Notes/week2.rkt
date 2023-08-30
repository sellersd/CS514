;;; CS 514 Assignment 2
;;; Author: David Sellers
;;; Date: 24AUG2023

#lang racket

(require "cs514-useful.rkt")
(require rackunit)

(define add1
  (lambda (a)
  (+ a 1)))
(define sub1
  (lambda (a)
  (- a 1)))

(add1 12)
(sub1 10)

(define plus
  (lambda (a b)
  (cond
    ((zero? b) a)
    (else (add1 (plus a (sub1 b)))))))

(plus 12 13)

(define minus
  (lambda (a b)
  (cond
    ((zero? b) a)
    (else (sub1 (minus a (sub1 b)))))))

(minus 12 5)
(minus 12 25)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))
      

(addtup '(1 2 3 4 5))

(define times
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (plus a (times a (sub1 b) ))))))

(times 2 3)
(times 10 12)
(times 32 32)


(define tup+ 
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
      ('()))
    (else
      (cons (plus (car tup1) (car tup2))
        (tup+ (cdr tup1) (cdr tup2)))
      ))))

(tup+ (1 2 3 4) (4 3 2 1))


