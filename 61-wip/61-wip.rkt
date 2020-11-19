#lang racket

(define (triangle n)
	(/ (+ n (* n n)) 2))

(define (square n)
	(* n n))

(define (pentagonal n)
	(/ (* n (- (* 3 n) 1)) 2))

(define (hexagonal n)
	(* n (- (* 2 n) 1)))

(define (heptagonal n)
	(/ (* n (- (* 5 n) 3)) 2))

(define (octagonal n)
	(* n (- (* 3 n) 2)))

(define (generate-numbers generator n upto numbers)
	(let ([current (generator n)])
		(if (> current upto)
			numbers
			(generate-numbers generator (+ n 1) upto (cons current numbers)))))

(define (gen generator)
	(let ([start 1]
		  [upto 10000]
		  [results (list)])
		(generate-numbers generator start upto results)))

(define (four-digit-only numbers)
	(filter (lambda (i) (> i 999)) numbers))

(four-digit-only (gen triangle))
(four-digit-only (gen square))
(four-digit-only (gen pentagonal))
(four-digit-only (gen hexagonal))
(four-digit-only (gen heptagonal))
(four-digit-only (gen octagonal))
