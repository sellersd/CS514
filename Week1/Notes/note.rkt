#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;; (atom? 'x)

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define x '(a (b (c d)) e f g))
;;; (display (lat? x))
;;; (display (lat? '(a b c d e f g h)))
;;; (display x)
;;; (display (car (cdr x)))

(pretty-print "Defining and executing member?")
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
		(member? a (cdr lat)))))))

;;; (member? 'a '(a b c d e))
;;; (member? 'g '(a b c d e))

(pretty-print "Defining and executing rember")
(define rember?
  (lambda (lat a)
    (cond
      ((null? lat) '())
      (else (eq? (car lat) a) (rember (cdr lat) a))
      
      )))


