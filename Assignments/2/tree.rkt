;;; Author: David Sellers
;;; Date: 25AUG2023
;;; Course: CS514 
;;; Assignment: 2 - Tree ADT
;;; References
;;; https://docs.racket-lang.org/reference/define-struct.html
;;; https://people.eecs.berkeley.edu/~bh/ssch18/trees.html
;;; https://rebelsky.cs.grinnell.edu/Courses/CSC151/2021Fa/readings/tree-structs.html
;;; https://courses.cs.washington.edu/courses/cse341/02wi/scheme/data-abstractions.html

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


(define (mk-tree node left right)
  (list  node left right))

(define (root tree) (car tree))
(define (leftsub tree) (car (cdr tree)))
(define (rightsub tree) (cdr (cdr tree)))

(define t1 (mk-tree 4 '() '()))
(mk-tree 4 3 5)

(check-eq? ((mk-tree 4 '() '()) (4 '() '())))
(check-eq? (mk-tree 4 3 5) (4 3 5))

(define insert
  (lambda (tat atom)
    (cond
      (empty-tree? '())
      ((< (root tat) atom) (displayln "less") (insert (leftsub tat) atom))
      ((> (root tat) atom) (displayln "more") (insert (rightsub tat) atom))
      (else (displayln "cons") (cons atom '())))))
        
(insert t1 3)
(displayln t1)
(root t1)
(leftsub t1)
(rightsub t1)

;;; (define node 'a)
;;; (displayln node)
;;; (define (tree node leftChild rightChild)
;;;   (lambda (a l r)
;;;     (cond
;;;       ((null? a) '())
;;;       (else (cons a '()))))) 

;;; (tree 2 3 4)
;;; (displayln (tree 2 3 4))
;;; (displayln tree)
;;; (displayln (car (cdr tree)))
