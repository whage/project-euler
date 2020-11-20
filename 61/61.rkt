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

(define (get-items-at-indexes lst indexes)
    (if (or (empty? lst) (empty? indexes))
        (list)
        (append (list (list-ref lst (first indexes))) (get-items-at-indexes lst (rest indexes)))))

(define (map-with-index fn lst idx)
    (if (empty? lst)
        empty
        (cons (fn idx) (map-with-index fn (cdr lst) (+ idx 1)))))

(define (all-options-as-pairs)
    (let ([index-set-pairs (map-with-index (lambda (idx) (cons idx (list-ref (all-sets) idx))) (all-sets) 0)])
        (map (lambda (index-set-pair)
            (map (lambda (number) (cons number (car index-set-pair))) (cdr index-set-pair))) index-set-pairs)))

(define (sort-option-pairs lists)
    (sort lists (lambda (a b) (<= (car a) (car b)))))

(define (get-available-options selected-values current-node-idx indexes-of-available-sets)
    (let* ([left-number (list-ref selected-values (max 0 (- current-node-idx 1)))]
          [current-number (list-ref selected-values current-node-idx)]
          [available-option-pairs (apply append (get-items-at-indexes (all-options-as-pairs) indexes-of-available-sets))]
          [sorted-option-pairs (sort-option-pairs available-option-pairs)])
        (if (= current-node-idx 0)
            (filter (lambda (p) (> (car p) current-number)) sorted-option-pairs)
            (if (= current-node-idx 5)
                (filter (lambda (p) (and (> (car p) current-number) (= (quotient (car p) 100) (modulo left-number 100)) (= (quotient (first selected-values) 100) (modulo (car p) 100)))) sorted-option-pairs)
                (filter (lambda (p) (and (> (car p) current-number) (= (quotient (car p) 100) (modulo left-number 100)))) sorted-option-pairs)))))

(define (get-available-sets used-sets unusable-sets)
    (sort (set->list (set-subtract (list->set (list 0 1 2 3 4 5)) (list->set used-sets) (list->set unusable-sets))) <))

(define (solve selected-values current-node-idx used-sets unusable-sets)
    (if (= current-node-idx -1)
        #f
        (let ([indexes-of-available-sets (get-available-sets used-sets unusable-sets)])
            (if (empty? indexes-of-available-sets)
                (solve (list-set selected-values current-node-idx 0) (- current-node-idx 1) (rest used-sets) (list)) ; backtrack
                (let ([options (get-available-options selected-values current-node-idx indexes-of-available-sets)])
                    (if (empty? options)
                        (solve selected-values current-node-idx used-sets (cons (first indexes-of-available-sets) unusable-sets)) ; try next set for current node
                        (if (= current-node-idx 5)
                            (list-set selected-values current-node-idx (car (first options)))
                            (solve (list-set selected-values current-node-idx (car (first options))) (+ 1 current-node-idx) (cons (cdr (first options)) used-sets) (list))))))))) ; pick first move on to next node

(trace solve)
(trace get-available-sets)
(apply + (solve (list 0 0 0 0 0 0) 0 (list) (list)))

;(get-items-at-indexes (list 10 20 30 40 50) (list 0 3 4))
;(all-options-as-pairs (get-items-at-indexes (all-sets) (list 4 5)))
;(sort-option-pairs (apply append (get-items-at-indexes (all-options-as-pairs) (list 4 5))))
