;;; Author: David Sellers
;;; Date: 03SEP2023
;;; Course: CS514 
;;; Assignment: 3 - Ribcage ADT
;;; References
;;; EOPL Problem 2.11
;;; https://github.com/mwand/eopl3
;;; https://docs.racket-lang.org/eopl/index.html

;;; Use textbook language
#lang eopl

;;; unit testing and course provided code
(require rackunit)
(require "cs514-useful.rkt")

;;; Note to self: the next three code 
;;; blocks are adaptions of eopl pg 38
;;;
;;; define an empty-env
(define empty-env
  (lambda () (list 'empty-env)))

;;; define extending the empty-env
;;; believe this to be analogous to 
;;; (variable value)* in BNF
;;; extends the environment by appending
;;; new environment i.e.
;;; (cons (extend-env new_vars new vals) current-environment)
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;;; apply the environment

