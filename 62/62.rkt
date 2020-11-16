#lang racket

(define (get-digits number acc)
    (if (= (quotient number 10) 0)
        (cons (modulo number 10) acc)
        (get-digits (quotient number 10) (cons (modulo number 10) acc))))

(define (get-sorted-digits number)
    (let ([acc (list)])
        (sort (get-digits number acc) <)))

(define (collect-cube-numbers dictionary n)
    (let* ([new-cube (* n n n)]
           [key (get-sorted-digits new-cube)]
           [value (hash-ref dictionary key (list))]
           [new-value (cons new-cube value)])
        (hash-set! dictionary key new-value)
        (if (= (length new-value) 5)
            (apply min new-value)
            (collect-cube-numbers dictionary (+ 1 n)))))

(collect-cube-numbers (make-hash) 2)
