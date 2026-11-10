
#lang racket

;; This line exports all of your defined functions,
;; so you can call them in hw5tests.rkt and so we can
;; call them in our tests.
;; Don't remove or comment out this line!
(provide (all-defined-out)) 

;; Implement your code below

;; #1
(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

;; keep going with the rest of the problems below

;;problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;problem 3
(define (list-nth-mod xs n)
  (cond [(negative? n)
    (error "list-nth-mod: negative number")]
  [(null? xs)
    (error "list-nth-mod: empty list")]
  [#t
    (car (list-tail xs (remainder n (length xs))))]))


;;problem 4
(define (stream-first-k-such-that f k s)
  (if (= k 0)
    null
    (let ([pr (s)])
      (if (f (car pr))
        (cons (car pr)
          (stream-first-k-such-that f
            (- k 1)
            (cdr pr)))
        (stream-first-k-such-that f
          k
          (cdr pr))))))

;;problem 5
(define funny-number-stream
  (let ()
    (define (f x)
      (cons (if (= (remainder x 6) 0)
        (- x)
        x)
        (lambda () (f (+ x 1)))))
    (lambda () (f 1))))


;;problem 6
(define dog-then-puppy
  (letrec ([dog (lambda ()
    (cons "dog.jpg" puppy))]
  [puppy (lambda ()
    (cons "puppy.jpg" dog))]) dog))


;;problem 7
(define (stream-add-one s)
  (lambda ()
    (let ([pr (s)])
      (cons (cons 1 (car pr))
        (stream-add-one (cdr pr))))))


;;problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
    (lambda ()
      (cons (cons (list-nth-mod xs n)
        (list-nth-mod ys n))
  (f (+ n 1)))))])
    (f 0)))

;;problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (i)
    (cond [(= i (vector-length vec)) #f]
      [(and (pair? (vector-ref vec i))
        (equal? (car (vector-ref vec i)) v))
        (vector-ref vec i)]
      [#t (f (+ i 1))]))])
    (f 0)))


;;problm 10
(define (caching-assoc xs n)
  (let ([cache (make-vector n #f)]
        [next-slot 0])
    (lambda (v)
      (let ([cached (vector-assoc v cache)])
        (if cached
          cached
          (let ([answer (assoc v xs)])
            (if answer
              (begin
                (vector-set! cache next-slot answer)
                (set! next-slot
                  (remainder (+ next-slot 1) n))
                answer)
              #f)))))))

;;problem 11
(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     ((lambda ()
        (define threshold e1)
        (define (loop)
          (if (> e2 threshold)
            (loop)
            #t))
        (loop)))]))
        









