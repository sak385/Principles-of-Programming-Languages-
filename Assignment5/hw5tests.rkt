#lang racket

(require "hw5.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 5")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (define row (quotient grid-posn pic-grid-width))
  (define col (remainder grid-posn pic-grid-width))
  (make-posn (+ border-size (* approx-pic-width col))
             (+ border-size (* approx-pic-height row))))

(define (place-picture window filename grid-posn)
  (define posn (grid-posn-to-posn grid-posn))
  ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
  ((draw-pixmap window) filename posn))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (define next (stream))
    (define filename (cdar next))
    (define grid-posn (caar next))
    (define next-stream (cdr next))
    (place-picture window filename grid-posn)
    (sleep pause)
    (place-repeatedly window pause next-stream (- n 1))))

;; Tests Start Here

;; Problem 1 tests
(displayln "Problem 1:")
(displayln (sequence 2 3 11))  ; should be '(3 5 7 9 11)
(displayln (sequence 3 3 8))   ; should be '(3 6)
(displayln (sequence 1 3 2))   ; should be '()

;; Problem 2 tests
(displayln "Problem 2:")
(displayln (string-append-map (list "a" "b" "c") "!"))
; should be '("a!" "b!" "c!")

;; Problem 3 tests
(displayln "Problem 3:")
(displayln (list-nth-mod (list "a" "b" "c") 0)) ; should be "a"
(displayln (list-nth-mod (list "a" "b" "c") 4)) ; should be "b"
(displayln (list-nth-mod (list 10 20 30) 8))     ; should be 30

;; Problem 4 tests
(displayln "Problem 4:")
(displayln
 (stream-first-k-such-that
  (lambda (x) (= (remainder x 2) 0))
  5
  funny-number-stream))
; should be '(2 4 8 10 14)

;; Problem 5 test
(displayln "Problem 5:")
(displayln
 (stream-first-k-such-that
  (lambda (x) #t)
  16
  funny-number-stream))
; should show the first 16 values of funny-number-stream

;; Problem 6 test
(displayln "Problem 6:")
(displayln
 (stream-first-k-such-that
  (lambda (x) #t)
  6
  dog-then-puppy))
; should be '("dog.jpg" "puppy.jpg" "dog.jpg" "puppy.jpg" "dog.jpg" "puppy.jpg")

;; Problem 7 test
(displayln "Problem 7:")
(displayln
 (stream-first-k-such-that
  (lambda (x) #t)
  4
  (stream-add-one dog-then-puppy)))
; should be '((1 . "dog.jpg") (1 . "puppy.jpg") (1 . "dog.jpg") (1 . "puppy.jpg"))

;; Problem 8 test
(displayln "Problem 8:")
(displayln
 (stream-first-k-such-that
  (lambda (x) #t)
  6
  (cycle-lists (list 1 2 3) (list "a" "b"))))
; should be '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b"))

;; Problem 9 tests
(displayln "Problem 9:")
(displayln (vector-assoc "a"
                         (vector 5 "hello" (cons "a" 1) (cons "b" 2))))
; should be '("a" . 1)

(displayln (vector-assoc "c"
                         (vector (cons "a" 1) (cons "b" 2))))
; should be #f

;; Problem 10 tests
(displayln "Problem 10:")
(define cached
  (caching-assoc
   (list (cons "a" 1)
         (cons "b" 2)
         (cons "c" 3))
   2))

(displayln (cached "a")) ; should be '("a" . 1)
(displayln (cached "b")) ; should be '("b" . 2)
(displayln (cached "a")) ; should still be '("a" . 1)
(displayln (cached "z")) ; should be #f



(displayln "Problem 11:")
(define a 5)
(while-greater 2 do
  (begin
    (set! a (- a 1))
    a))
(displayln a)
; should print 2












; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "dog" "puppy" "curry") 
               ".jpg"))

(define funny-test (stream-first-k-such-that (lambda (x) #t) 16 funny-number-stream))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dog-then-puppy) 27))
