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

(four-digit-only (gen triangle))
(four-digit-only (gen square))
(four-digit-only (gen pentagonal))
(four-digit-only (gen hexagonal))
(four-digit-only (gen heptagonal))
(four-digit-only (gen octagonal))

(define (get-available-options current-value options)
    (if (= current-value 0)
        options
        (filter (lambda (n) (= (quotient n 100) (modulo current-value 100))) options)))

(define (get-available-sets used-sets)
    (sort (set->list (set-subtract (list->set (list 0 1 2 3 4 5)) (list->set used-sets))) <))

(define (solve selected-values current-node-idx used-sets)
    (if (= current-node-idx -1)
        #f
        (let ([indexes-of-available-sets (get-available-sets used-sets)])
            (if (empty? indexes-of-available-sets)
                (solve (list-set selected-values current-node-idx 0) (- current-node-idx 1) (rest used-sets)) ; backtrack
                (let* ([first-available-set (list-ref (all-sets) (first indexes-of-available-sets))]
                       [options (get-available-options (list-ref selected-values (max 0 (- current-node-idx 1))) first-available-set)])
                    (if (empty? options)
                        ;(solve (list-set selected-values current-node-idx 0) (- current-node-idx 1) (rest used-sets)) ; backtrack
                        (solve selected-values current-node-idx (cons (first indexes-of-available-sets) used-sets))
                        (if (= current-node-idx 5)
                            (list-set selected-values current-node-idx (first options))
                            (solve (list-set selected-values current-node-idx (first options)) (+ 1 current-node-idx) (cons (first indexes-of-available-sets) used-sets)))))))))

(trace solve)
(trace get-available-sets)
;(trace get-available-options)
(solve (list 0 0 0 0 0 0) 0 (list))
