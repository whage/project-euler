#lang racket

(require racket/trace)

(define constraint-0 
	(lambda (_)
		(lambda (i) i)))

(define constraint-1 
	(lambda (_)
		(lambda (i) i)))

(define constraint-2 
	(lambda (_)
		(lambda (i) i)))

(define constraint-3
	(lambda (selected-values) ; a function
		(lambda (candidate) ; that returns a filtering function
			(let ([sum-of-first-3 (apply + (take selected-values 3))])
				(> sum-of-first-3 (+ candidate (third selected-values)))))))

(define constraint-4
	(lambda (selected-values) ; a function
		(lambda (candidate) ; that returns a filtering function
			(let ([sum-of-first-3 (apply + (take selected-values 3))])
				(= sum-of-first-3 (+ candidate (third selected-values) (fourth selected-values)))))))

(define constraint-5 
	(lambda (_)
		(lambda (i) i))); there is nothing left to filter

(define constraints (list constraint-0 constraint-1 constraint-2 constraint-3 constraint-4 constraint-5))

(define (filter-options node-idx options selected-values)
	(let ([constraint-fn (list-ref constraints node-idx)])
		(filter (constraint-fn selected-values) options)))

(define (solve selected-values current-node-idx remaining-numbers)
	;(if (and (= current-node-idx 5) (= (length remaining-numbers) 1))
	(if (= current-node-idx 5)
		(list-set selected-values current-node-idx (list-ref remaining-numbers 0))
		(let ([options (filter-options current-node-idx remaining-numbers selected-values)])
			(if (not (empty? options))
				(solve (list-set selected-values current-node-idx (car options)) (+ 1 current-node-idx) (remove (car options) remaining-numbers))
				(list 0 0 0 0 0 0)))))

(trace filter-options)
(trace solve)
(solve (list 0 0 0 0 0 0) 0 (list 1 2 3 4 5 6))

;(filter (constraint-3 (list 1 3 2 0 0 0)) (list 4 5 6))
;(filter (constraint-3 (list 1 5 2 0 0 0)) (list 3 4 6))

;(filter (constraint-4 (list 1 2 3 4 0 0)) (list 5 6))
;(filter (constraint-4 (list 1 2 3 5 0 0)) (list 4 6))
;(filter (constraint-4 (list 1 2 3 6 0 0)) (list 4 5))
;(filter (constraint-4 (list 1 2 4 3 0 0)) (list 5 6))
;(filter (constraint-4 (list 1 2 4 5 0 0)) (list 3 6))
;(filter (constraint-4 (list 1 2 4 6 0 0)) (list 3 5))
;(filter (constraint-4 (list 1 2 5 3 0 0)) (list 4 6))
;(filter (constraint-4 (list 1 2 5 4 0 0)) (list 3 6))
;(filter (constraint-4 (list 1 2 5 6 0 0)) (list 3 4))
;(filter (constraint-4 (list 1 2 6 3 0 0)) (list 4 5))
;(filter (constraint-4 (list 1 2 6 4 0 0)) (list 3 5))
;(filter (constraint-4 (list 1 2 6 5 0 0)) (list 3 4))
;(filter (constraint-4 (list 1 3 2 4 0 0)) (list 5 6))
;(filter (constraint-4 (list 1 3 2 5 0 0)) (list 4 6))
;(filter (constraint-4 (list 1 3 2 6 0 0)) (list 4 5))
;(filter (constraint-4 (list 1 3 4 2 0 0)) (list 5 6))
;(filter (constraint-4 (list 1 3 4 5 0 0)) (list 2 6))
;(filter (constraint-4 (list 1 3 4 6 0 0)) (list 2 5))
;(filter (constraint-4 (list 1 3 5 2 0 0)) (list 4 6))
;(filter (constraint-4 (list 1 3 5 4 0 0)) (list 2 6))
;(filter (constraint-4 (list 1 3 5 6 0 0)) (list 2 4))
;(filter (constraint-4 (list 1 3 6 2 0 0)) (list 4 5))
;(filter (constraint-4 (list 1 3 6 4 0 0)) (list 2 5))
;(filter (constraint-4 (list 1 3 6 5 0 0)) (list 2 4))

