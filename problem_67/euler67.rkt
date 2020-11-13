#lang racket

(define (map-with-index fn lst idx)
    (if (empty? lst)
        empty
        (cons (fn idx) (map-with-index fn (cdr lst) (+ idx 1)))))

(define single-line-number-triangle
	(list
		(list 3)))


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
	(apply max (max-of-last-line number-triangle)))

;(reverse (cdr (reverse '(1 2 3 4 5))))

(define (max-of-last-line number-triangle)
	(if (equal? 1 (length number-triangle))
		(first number-triangle)
		(max-of-2-lines
			(last number-triangle)
			(max-of-last-line (reverse (cdr (reverse number-triangle)))))))

(define (max-of-2-lines bottom top)
    (map-with-index (lambda (idx)
        (if (= idx 0)
            (+ (first top) (first bottom))
            (max (+ (list-ref bottom idx) (if (= idx (length top)) 0 (list-ref top idx)))
                 (+ (list-ref bottom idx) (list-ref top (- idx 1)))))) bottom 0))

;(last two-line-number-triangle)
;(max-of-last-line (reverse (cdr (reverse two-line-number-triangle))))
;(max-of-2-lines (list 7 4) (list 3))

;(max-path-sum single-line-number-triangle)
(max-path-sum small-number-triangle)
;(reverse (cdr (reverse two-line-number-triangle)))
