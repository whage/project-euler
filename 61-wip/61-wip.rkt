#lang racket

(require racket/trace)

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
          [upto 9999]
          [results (list)])
        (generate-numbers generator start upto results)))

(define (four-digit-only numbers)
    (filter (lambda (i) (> i 999)) numbers))

(define (all-sets)
    (list
        (four-digit-only (gen triangle))
        (four-digit-only (gen square))
        (four-digit-only (gen pentagonal))
        (four-digit-only (gen hexagonal))
        (four-digit-only (gen heptagonal))
        (four-digit-only (gen octagonal))))

(all-sets)

(define (filter-options node-idx options selected-values)
    ;(let ([constraint-fn (list-ref constraints node-idx)])
    (let ([constraint-fn constraint-0])
        (filter (constraint-fn selected-values) options)))

(define constraint-0
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            #t)))

(define constraints
    (list
        constraint-0))

(define (solve selected-values current-node-idx remaining-sets)
    (if (= current-node-idx -1)
        #f
        (let ([options (filter-options current-node-idx remaining-sets selected-values)])
            (if (empty? options)
                (solve (list-set selected-values current-node-idx 0) (- current-node-idx 1) (get-remaining (take selected-values (max 0 (- current-node-idx 1)))))
                (if (= current-node-idx 9)
                    (list-set selected-values current-node-idx (car options))
                    (solve (list-set selected-values current-node-idx (car options)) (+ 1 current-node-idx) (remove (car options) remaining-sets)))))))
