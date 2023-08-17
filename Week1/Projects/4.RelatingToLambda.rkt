#lang racket

;;;
;;; File:   RelatingToLambda.rkt
;;; Author: Your Glorious Instructor
;;; Purpose:
;;; Continue working through The Little Schemer, starting from Chapter 7

(require "cs514-useful.rkt")
(require rackunit)

;;;
;;; Remember the Seventh Commandment: Recur on subparts of the same nature
;;;


(define firstset
  (lambda (lat)
  (cond
    ((null? lat) #t)
    (else
     (cond
       ((member? (car lat) (cdr lat)) #f)
       (else (firstset (cdr lat))))))))

(check-false (firstset '(a b a c d e)))
(check-true (firstset '(a b c d e)))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(check-false (set? '(apple 3 pear 4 9 apple 3 4)))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(check-equal? (makeset '(apple peach pear peach plum apple lemon peach))
             '(pear plum apple lemon peach))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset? (cdr set1) set2))
      (else #f))))

(define s1 '(5 chicken wings))
(define s2 '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(check-true (subset? s1 s2))
                     

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;;; See if you can optimize this code.
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (cond
         ((member? (car set1) set2) #t)
         (else (intersect? (cdr set1) set2)))))))

(check-true (intersect? '(stewed tomatoes and macaroni)
                        '(macroni and cheese)))

;;; Now to actually produce the intersection, and again can you
;;; optimize this?

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect s1 s2)

;;; OK, let's do union
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(union s1 s2)

;;; And do set difference
(define setdiff
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2)
       (setdiff (cdr set1) set2))
      (else (cons (car set1) (setdiff (cdr set1) set2))))))

;;; and mult-way intersect
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(check-equal? (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))
(check-equal? (intersectall
              '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))
              '(6 and))

;;; And now we pair off into the pair datatype.

;;; Don't fail into the trap of thinking in C++. Remember that we
;;; defined as a list of numbers.  This is different from how C++
;;; defined tuples, and then pairs.   Here, we will be more precise and
;;; say that a pair is a list with only two S-expressions.
;;;
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(check-true (a-pair? '(3 7)))
(check-true (a-pair? '((2) (pair))))
(check-true (a-pair? '(full (house))))

;;; Let's make this easier
(define first (lambda (p) (cond (else (car p)))))
(define second (lambda (p) (cond (else (car (cdr p))))))
(define build
  (lambda (s1 s2)
    (cond
      (else (cons s1 (cons s2 '()))))))

(define third (lambda (l) (car (cdr (cdr l)))))

;;; Relations and functions. The objective is to see if we can build a
;;; definition in Racket that matches the definitions used in Discrete
;;; Math.
;;; Start with noting that both are lists of pairs
;;; So a finite funciton is a list of pairs in which no first element of any
;;; pair is the same as any other element.

(define fun? (lambda (rel) (set? (firsts rel))))

;;; Now define an expression that reverses a relation
(define revrel1
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (build (second (car rel)) (first (car rel)))
                  (revrel1 (cdr rel)))))))

;;; But a helper function is in order
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;;; and we can simplify
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(check-equal? (revrel '((2 3) (3 2))) '((3 2) (2 3)))

;;; Now to play with functions (at least our definition)
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(check-true (one-to-one? '((chocolate chip) (doughy cookie))))

;;; Now we find ourselves facing the Racket equivalent of funciton
;;; pointers, function objects, and (Dun-dun-dun!) lambda expressions.
;;; Even better as we are using a language in the LISP family of
;;; languages as LISP is where the concept of "lambda expressions" comes
;;; from!

;;;  Consider the following, noting how test? is used...

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      (else (cond
              ((test? (car l) a) (cdr l))
              (else (cons (car l) (rember-f test? a (cdr l)))))))))

;;; Do take note of the footnote in the book about using funcall if
;;; there is a possibility that test? has yet to be defined.

(define a 'jelly)
(define l '(jelly beans are good))
(check-equal? (rember-f eq? a l) '(beans are good))

;;  Now things get funky... consider the return type of this expression

(lambda (a)
  (lambda (x)
    (eq? x a)))

;; The term for this is "curry-ing" a function.  Now note what we do here:

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;;; The define function establishes that connection between the name
;;; "eq?-c" and the procedure defined by the outer lambda, which by the
;;; definition in the inner lambda returns a procedure.  This is the
;;; same concept of function as data that C and C++ has struggled with
;;; given the concepts of function pointers, functors, and anonymous
;;; functions.  LISP was here first!

;;; Look how this lets us rewrite the rember-f function:

(define rember-f-anon
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f-anon test?) a
                                                   (cdr l))))))))


((rember-f-anon eq?) 'tuna '(shrimp salad and tuna salad))
(check-equal?
 ((rember-f-anon eq?) 'tuna '(shrimp salad and tuna salad))
 '(shrimp salad and salad))

;;;
;;; Now think about what the result is of the following:
((rember-f-anon eq?) 'eq? '(equal? eq? eqlist? eqpair?))
;;; Two important notes:
;;; 1.  Parse the expression, remembering what the return value is from
;;; rember-f-anon
;;; 2.  What's the difference between the different "eq?" things in this
;;; form? 

;;; You know... I really like curry.  Let's see how the combination of
;;; currying and lambda can be used to generalize code.  Look at the
;;; test and note the structure.   `insertL-f` returns a lambda
;;; expression (anonymous function, right?).  So, the inner s-exp is the
;;; actual call to `insertL-f`, and then we call the function returned
;;; by `insertL-f` with the listed parameters.

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else
          (cons (car l) ((insertL-f test?) new old (cdr l))))))))

; Test insertL-f
;
((insertL-f eq?)
 'd
  'e
  '(a b c e f g d h))                  ; '(a b c d e f g d h)

; The insertR function, curried
;
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else
          (cons (car l) ((insertR-f test?) new old (cdr l))))))))

; Test insertR-f
((insertR-f eq?)
  'e
  'd
  '(a b c d f g d h))                  ; '(a b c d e f g d h)

;;;
;;; Where do these two functions differ? Only in the middle!  As the
;;; book says: "The two functions cons old and new in a different order
;;; onto the cdr of the list l."  So, define functions we can pass into
;;; an insert routine to make things work:

(define seqL
  (lambda (new old l)               ; a lambda exp. taking three args
    (cons new                       ; consing the first argument
          (cons old l))))           ; onto the result of consing the
                                    ; second onto the third

;;; Can you explain what the following function does?
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

;;; And now we can write an insert function that works in either
;;; direction!  Talk through the implication, noting what value
;;; is returned from insert-g 

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

;;; Insert left and insert right become one liners

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

;;; Of course, I could have just put function defintions directed into
;;; insertL or insertR.  LISP programmers would say do so as its less
;;; bound names but I think it messes with readability of code.   That's
;;; always been a compliant about LISP variants.

;;; Consider:

(define subst
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((eq? (car l) old)
       (cons new (cdr l)))
      (else (cons (car l)
                  (subst new old (cdr l)))))))

;;; This looks a lot like what we did at first for insertL and insertR!
;;; With the right helper function, we can implement subst with
;;; insert-g!

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst-g
  (insert-g seqS))

;;; And how about this one!

(define seqrm (lambda (new old l) l))

(define rember-g
  (lambda (a l)
    ((insert-g seqrm) #f a l)))

;;; What is the code doing with the #f?  Walk through what happens with
;;; the following:
(define anAtom 'sausage)
(define aList '(pizza with sausage and bacon))
(rember-g anAtom aList)

;;;
;;; A short degression:
;;; TLS is a lot older than one might expect, with it dating from the
;;; later 1980s and Common Lisp.    So the stuff in the book doesn't use
;;; a lot of the local binding forms that are provided in more recent
;;; LISP dialects like Scheme, Racket, and Emacs LISP.

;;; A better way to use the local binding forms, which define closures
;;; in Scheme dialects like Racket (and is used in most modern forms of
;;; LISP

;;; Here's a better way to phrase the testing form we did just before
;;; this point:

(let ([anAtom 'sausage] [aList '(pizza with sausage and bacon)])
 (rember-g anAtom aList))

;;;
;;; The let form binds a set of identifiers, each to some expression for
;;; use in the body of the form.    This is done "in parallel" in let.
;;; That means that none of ids are bound in the right hand expr for any
;;; id but are all available in the body.

;;; The let* form operates serially, where each id in the list of ids is
;;; available for use in later expression. Note this means the ids do
;;; not need be distinct and the most recent binding is the most recent:

(let* ([name (list "Burroughs")]
         [name (cons "Rice" name)]
         [name (cons "Edgar" name)])
  name)

;;;
;;; Back to the regular program...
;;;

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

;;; Now let's curry that thing...

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))
        ((test? a (car lat))
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

(let ([test? eq?] [a 'tuna] [lat '(shrimp salad tuna salad and tuna)])
  (check-equal? ((multirember-f test?) a lat) '(shrimp salad salad and))
  )

;;; And thus,

(define multirember-eq? (multirember-f eq?))

;;;
;;; And here's things get... interesting
;;;

(define multiremember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multiremember&co a (cdr lat)
       (lambda (newlat seen)
         (col newlat (cons (car lat) seen)))))
      (else
        (multiremember&co a (cdr lat)
                          (lambda (newlat seen)
                            (col (cons (car lat) newlat) seen)))))))

;;; And now to show what's happening

(define a-friend
  (lambda (x y) (null? y)))

(let ([a 'tuna] [lat '(strawberries tuna and swordfish)])
  (multiremember&co a lat a-friend)
  )

;;; Let's try something less fraught

(let ([a 'tuna] [lat '(tuna)])
  (begin
    (multiremember&co a '() a-friend)
    (multiremember&co a lat a-friend)
    )
  )

;;; BTW, the begin form is how you things in sequential order in Racket.
;;; Think about this works on those two cases.

;;; Try this:
(define print-a-friend
  (lambda (x y)
    (begin
      (pretty-print (cons x y))
      (null? y)
      )))

(let ([a 'tuna] [lat '(tuna)])
  (begin
    (pretty-print "Printing a friend with co")
    (multiremember&co a '() print-a-friend)
    (multiremember&co a lat print-a-friend)
    )
  )

;;;
;;; Here we see the first example of a collector (or the more general
;;; concept we'll look at later in the term of a "continuation"
;;;
;;; Consider what `(multirember&co a lat f) does:  Check every atom in
;;; that lat to see if is `eq?` to `a.  Items that are not go into one
;;; list and those that are into another.  Finally, we get the value of
;;; applying the function to the two lists.

;;; Earlier we defined two functions to multi-insert left and
;;; multi-insert right.   Let's combine the two:

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else
        (cons
          (car lat)
          (multiinsertLR new oldL oldR (cdr lat)))))))

; Example of multiinsertLR
;
(check-equal?
 (multiinsertLR
  'x
  'a
  'b
  '(a o a o b o b b a b o))
 '(x a o x a o b x o b x b x x a b x o))

; The multiinsertLR&co is to multiinsertLR what multirember is to
; multiremember&co
;
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (+ 1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L (+ 1 R)))))
      (else
        (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat)
                                 L R)))))))

; Some collectors
;
(define col1
  (lambda (lat L R)
    lat))
(define col2
  (lambda (lat L R)
    L))
(define col3
  (lambda (lat L R)
    R))

; Examples of multiinsertLR&co
;
(check-equal?
 (multiinsertLR&co
  'salty
  'fish
  'chips
  '(chips and fish or fish and chips)
  col1)
 '(chips salty and salty fish or salty fish and chips salty))

(check-equal?
 (multiinsertLR&co
  'salty
  'fish
  'chips
  '(chips and fish or fish and chips)
  col2)
 2)

(check-equal?
 (multiinsertLR&co
  'salty
  'fish
  'chips
  '(chips and fish or fish and chips)
  col3)
 2)

;;; So... The Tenth Commandment:
;;; Build functions to collect more than one value at a time.
;;;

; The evens-only* function leaves all even numbers in an sexpression
; (removes odd numbers)
;
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else
           (evens-only* (cdr l)))))
      (else
        (cons (evens-only* (car l))
              (evens-only* (cdr l)))))))

; Example of evens-only*
;
(evens-only*
  '((9 1 2 8) 3 10 ((9 9) 7 6) 2))  ; '((2 8) 10 (() 6) 2)

; Evens only function with a collector, collects evens, their product,
; and sum of odd numbers
;
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl) (* (car l) p) s))))
         (else
           (evens-only*&co (cdr l)
                           (lambda (newl p s)
                             (col newl p (+ (car l) s)))))))
      (else
        (evens-only*&co (car l)
                        (lambda (al ap as)
                          (evens-only*&co (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (* ap dp)
                                                 (+ as ds))))))))))

; evens-friend returns collected evens
;
(define evens-friend
  (lambda (e p s)
    e))

; Example of evens-friend used
;
(evens-only*&co 
  '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
  evens-friend)
; ==> '((2 8) 10 (() 6) 2)

; evens-product-friend returns the product of evens
;
(define evens-product-friend
  (lambda (e p s)
    p))

; Example of evens-product-friend used
;
(evens-only*&co 
  '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
  evens-product-friend)
; ==> 1920

; evens-sum-friend returns the sum of odds
;
(define evens-sum-friend
  (lambda (e p s)
    s))

; Example of evens-sum-friend used
;
(evens-only*&co 
  '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
  evens-sum-friend)
; ==> 38

; the-last-friend returns sum, product and the list of evens consed together
;
(define the-last-friend
  (lambda (e p s)
    (cons s (cons p e))))

; Example of the-last-friend
;
(evens-only*&co 
  '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
  the-last-friend)
; ==> '(38 1920 (2 8) 10 (() 6) 2)


;;;
;;; Collectors are a pretty common design pattern in languages.   For
;;; example, a number of the functions in the C++ STL algorithms library
;;; allow you to provide a callable object (function pointer, functor,
;;; or anonymous function) to use a a collector on a collection.   Once
;;; again, we see where LISP (and it's descendants like Scheme and
;;; Racket) did it first.
;;;

;;;
;;; Understanding collectors (well precisely, continuations) is a
;;; critical aspect of functional programming.   




