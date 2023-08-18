#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'x)

;;; (define lat?
;;;   (lambda (l)
;;;     (cond 
;;;       ((null? l) #t)
;;;       ((atom? (car l) (lat? (cdr l))))

(define x '(a (b (c d)) e f g))
(display x)
(display (car (cdr x)))



