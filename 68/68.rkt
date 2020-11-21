#lang racket

(require racket/trace)

; constraint for node 0
(define constraint-0
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (> candidate (list-ref selected-values 0)))))

; constraint for node 1
(define constraint-1
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (> candidate (list-ref selected-values 1)))))

; constraint for node 2
(define constraint-2
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (> candidate (list-ref selected-values 2)))))

; constraint for node 3
(define constraint-3
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 2))])
                (and (> sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 3)))))))

; constraint for node 4
(define constraint-4
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 2) (list-ref selected-values 3))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 4)))))))

; constraint for node 5
(define constraint-5
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 4))])
                (and (> sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 5)))))))

; constraint for node 6
(define constraint-6
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 4) (list-ref selected-values 5))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 6)))))))

; constraint for node 7
(define constraint-7
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 6))])
                (and (> sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 7)))))))

; constraint for node 8
(define constraint-8
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 6) (list-ref selected-values 7))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 8)))))))

; constraint for node 9
(define constraint-9
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 8) (list-ref selected-values 1))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 9)))))))

; groups together all the constraints
(define constraints
    (list
        constraint-0
        constraint-1
        constraint-2
        constraint-3
        constraint-4
        constraint-5
        constraint-6
        constraint-7
        constraint-8
        constraint-9))

; returns the readout of the chosen set of numbers as described by the problem
(define (get-readout numbers)
    (list 
        (list-ref numbers 0)
        (list-ref numbers 1)
        (list-ref numbers 2)

        (list-ref numbers 3)
        (list-ref numbers 2)
        (list-ref numbers 4)

        (list-ref numbers 5)
        (list-ref numbers 4)
        (list-ref numbers 6)

        (list-ref numbers 7)
        (list-ref numbers 6)
        (list-ref numbers 8)

        (list-ref numbers 9)
        (list-ref numbers 8)
        (list-ref numbers 1)))

; gets the remaining numbers: returns the set-difference of the given list and the base set
(define (get-remaining list-b)
    (let ([base (list 1 2 3 4 5 6 7 8 9 10)])
        (sort (set->list (set-subtract (list->set base) (list->set list-b))) <)))

; takes the constarint function for the given node and applies filter to it and the remaining options
(define (filter-options node-idx options selected-values)
    (let ([constraint-fn (list-ref constraints node-idx)])
        (filter (constraint-fn selected-values) options)))

; implements backtracing
(define (solve selected-values current-node-idx remaining-numbers)
    (if (= current-node-idx -1) ; if we had to backtrack from the first node then there is no solution
        #f
        (let ([options (filter-options current-node-idx remaining-numbers selected-values)]) ; get possible options for current node
            (if (empty? options) ; if there are no options satisfying the constraints
                (solve (list-set selected-values current-node-idx 0) (- current-node-idx 1) (get-remaining (take selected-values (max 0 (- current-node-idx 1))))) ; backtrack
                (if (= current-node-idx 9) ; if there are options left and node index is 9 that means we found a solution
                    (begin
                        (write (get-readout (list-set selected-values current-node-idx (car options)))) ; print solution
                        (display "\n") ; and a new line
                        (solve (list-set selected-values current-node-idx (car options)) current-node-idx (remove (car options) remaining-numbers))) ; continue search
                    (solve (list-set selected-values current-node-idx (car options)) (+ 1 current-node-idx) (remove (car options) remaining-numbers))))))) ; pick the first option and move to next node

;(trace get-remaining)
;(trace filter-options)
;(trace solve)
(solve (list 0 0 0 0 0 0 0 0 0 0) 0 (list 1 2 3 4 5 6 7 8 9 10))
