#lang racket

(define two-line-number-triangle
	(list
		(list 3)
		(list 7 4)))

(define small-number-triangle
	(list
		(list 3)
		(list 7 4)
		(list 2 4 6)
		(list 8 5 9 3)))

(define (max-path-sum number-triangle)
	((max-of-last-line number-triangle)))

;(reverse (cdr (reverse '(1 2 3 4 5))))

(define (max-of-last-line number-triangle)
	(if (equal 1 (length number-triangle))
		(apply max (first number-triangle))
		(if (equal 2 (length number-triangle))
			(max-of-2-lines (first number-triangle) (last number-triangle))
			(max-of-line-and-previous-lines
				(last number-triangle)
				(reverse (cdr (reverse number-triangle)))))))

(define (max-of-2-lines bottom top)
	)

;(define (max-of-line-and-previous-lines line previous-lines)
	

- max = utolsó sor maximuma

- fogod az előző sor maximum értékeit
	- minden i => e elemre ebben a sorban
		- max = max(e+előző[i], e+előző[i-1])
