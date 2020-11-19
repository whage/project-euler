#lang racket

(require racket/trace)

(define constraint-0
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (> candidate (list-ref selected-values 0)))))

(define constraint-1
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (> candidate (list-ref selected-values 1)))))

(define constraint-2
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (> candidate (list-ref selected-values 2)))))

(define constraint-3
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 2))])
                (and (> sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 3)))))))

(define constraint-4
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 2) (list-ref selected-values 3))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 4)))))))

(define constraint-5
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 4))])
                (and (> sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 5)))))))

(define constraint-6
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 4) (list-ref selected-values 5))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 6)))))))

(define constraint-7
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 6))])
                (and (> sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 7)))))))

(define constraint-8
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 6) (list-ref selected-values 7))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 8)))))))

(define constraint-9
    (lambda (selected-values) ; a function
        (lambda (candidate) ; that returns a filtering function
            (let ([sum-of-first-3 (apply + (take selected-values 3))]
                  [sum-with-candidate (+ candidate (list-ref selected-values 8) (list-ref selected-values 1))])
                (and (= sum-of-first-3 sum-with-candidate) (> candidate (list-ref selected-values 9)))))))

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

(define (get-remaining list-b)
    (let ([base (list 1 2 3 4 5 6 7 8 9 10)])
        (sort (set->list (set-subtract (list->set base) (list->set list-b))) <)))
    

(define (filter-options node-idx options selected-values)
    (let ([constraint-fn (list-ref constraints node-idx)])
        (filter (constraint-fn selected-values) options)))

(define (solve selected-values current-node-idx remaining-numbers)
    (if (= current-node-idx -1)
        #f
        (let ([options (filter-options current-node-idx remaining-numbers selected-values)])
            (if (empty? options)
                (solve (list-set selected-values current-node-idx 0) (- current-node-idx 1) (get-remaining (take selected-values (max 0 (- current-node-idx 1)))))
                (if (= current-node-idx 9)
                    (begin
                        ;(write (list-set selected-values current-node-idx (car options)))
                        (write (get-readout (list-set selected-values current-node-idx (car options)))) ; print solution
                        (display "\n")
                        (solve (list-set selected-values current-node-idx (car options)) current-node-idx (remove (car options) remaining-numbers))) ; continue search
                    (solve (list-set selected-values current-node-idx (car options)) (+ 1 current-node-idx) (remove (car options) remaining-numbers)))))))

;(trace get-remaining)
;(trace filter-options)
;(trace solve)
(solve (list 0 0 0 0 0 0 0 0 0 0) 0 (list 1 2 3 4 5 6 7 8 9 10))
