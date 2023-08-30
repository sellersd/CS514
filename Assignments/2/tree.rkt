;;; Author: David Sellers
;;; Date: 25AUG2023
;;; Course: CS514 
;;; Assignment: 2 - Tree ADT
;;; References
;;; https://docs.racket-lang.org/reference/define-struct.html
;;; https://people.eecs.berkeley.edu/~bh/ssch18/trees.html
;;; https://rebelsky.cs.grinnell.edu/Courses/CSC151/2021Fa/readings/tree-structs.html

#lang racket

(require rackunit)
(require "cs514-useful.rkt")

;;; Defines empty tree and predicate for checking empty tree
(define empty-tree '())
(define empty-tree?
  (lambda (t)
    (null? t)))

(check-eq? (empty-tree? '()) #t)
(check-eq? (empty-tree? '8) #f)
(check-eq? (empty-tree? '(8)) #f)


(define (tree node leftChild rightChild)
  (lambda (a l r)
    (cond
      ((null? a) '())
      (else (cons a '()))))) 

(tree 2 3 4)
(displayln (tree 2 3 4))
;;; (displayln tree)
;;; (displayln (car (cdr tree)))
