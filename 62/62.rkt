#lang racket

; gets the digits of the given number as a list
(define (get-digits number acc)
    (if (= (quotient number 10) 0)
        (cons (modulo number 10) acc)
        (get-digits (quotient number 10) (cons (modulo number 10) acc))))

; wrapper for getting digits and sorting them
(define (get-sorted-digits number)
    (let ([acc (list)])
        (sort (get-digits number acc) <)))

; collects cube numbers into a dictionary tha is indexed by the sorted digits
; and the values are lists of numbers with those digits
(define (collect-cube-numbers dictionary n)
    (let* ([new-cube (* n n n)]
           [key (get-sorted-digits new-cube)]
           [numbers (hash-ref dictionary key (list))] ; get existing list at key (or an empty list)
           [new-numbers (cons new-cube numbers)]) ; add number to list
        (hash-set! dictionary key new-numbers)
        (if (= (length new-numbers) 5)
            (apply min new-numbers) ; return the lowest of all numbers at the given key
            (collect-cube-numbers dictionary (+ 1 n)))))

(collect-cube-numbers (make-hash) 2)
