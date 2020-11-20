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

(define (generate-numbers generator n upto)
    (let ([current (generator n)])
        (if (> current upto)
            (list)
            (append (list current) (generate-numbers generator (+ n 1) upto)))))

(define (gen generator)
    (let ([start 1]
          [upto 9999])
        (generate-numbers generator start upto)))

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

(define (get-available-options selected-values current-node-idx options)
    (let ([left-number (list-ref selected-values (max 0 (- current-node-idx 1)))]
          [current-number (list-ref selected-values current-node-idx)])
        (if (= current-node-idx 0)
            (filter (lambda (n) (> n current-number)) options)
            (if (= current-node-idx 5)
                (filter (lambda (n) (and (> n current-number) (= (quotient n 100) (modulo left-number 100)) (= (quotient (first selected-values) 100) (modulo n 100)))) options)
                (filter (lambda (n) (and (> n current-number) (= (quotient n 100) (modulo left-number 100)))) options)))))

(define (get-available-sets used-sets unusable-sets)
    (sort (set->list (set-subtract (list->set (list 0 1 2 3 4 5)) (list->set used-sets) (list->set unusable-sets))) <))

(define (solve selected-values current-node-idx used-sets unusable-sets)
    (if (= current-node-idx -1)
        #f
        (let ([indexes-of-available-sets (get-available-sets used-sets unusable-sets)])
            (if (empty? indexes-of-available-sets)
                (solve (list-set selected-values current-node-idx 0) (- current-node-idx 1) (rest used-sets) (list)) ; backtrack
                (let* ([first-available-set (list-ref (all-sets) (first indexes-of-available-sets))]
                       [options (get-available-options selected-values current-node-idx first-available-set)])
                    (if (empty? options)
                        (solve selected-values current-node-idx used-sets (cons (first indexes-of-available-sets) unusable-sets)) ; try next set for current node
                        (if (= current-node-idx 5)
                            (list-set selected-values current-node-idx (first options))
                            (solve (list-set selected-values current-node-idx (first options)) (+ 1 current-node-idx) (cons (first indexes-of-available-sets) used-sets) (list))))))))) ; pick first move on to next node

(trace solve)
(trace get-available-sets)
(solve (list 0 0 0 0 0 0) 0 (list) (list))
