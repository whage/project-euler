#lang racket

;(define (get-count-for-given-power power)
;	(let ([base 1]
;		  [acc 0])
;		(if (= base 9))))

(define (get-count-for-constant-base base)
	(let ([initial-power 1]
		  [initial-acc 0])
		(define (iterate-power power acc)
			(if (>= base 10)
				acc
				(if (>= (digit-count (expt base power)) power)
					(iterate-power (+ 1 power) (+ 1 acc))
					acc)))
		(iterate-power initial-power initial-acc)))
	

(define (get-count-for-constant-power base power acc)
	(if (or (>= base 10) (< power 1))
		acc
		(if (>= (digit-count (expt base power)) power)
			(get-count-for-constant-power (+ 1 base) power (+ 1 acc))
			acc)))

(define (digit-count n)
	(let ([acc 1])
		(if (< n 10)
			acc
			(+ acc (digit-count (/ n 10))))))

;(digit-count 12341234)

;(get-count-for-constant-power 1)

(get-count-for-constant-base 9)
(get-count-for-constant-power 1 1 0)
