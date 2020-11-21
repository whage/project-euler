#lang racket

; returns number of numbers that when raised to `power` are also of length `power`
(define (get-count-for-constant-power power)
    (let ([initial-base 1]
          [initial-acc 0])
        (define (iterate-base base acc)
            (if (>= base 10)
                acc
                (if (< (digit-count (expt base power)) power)
                    (iterate-base (+ 1 base) acc)
                    (iterate-base (+ 1 base) (+ 1 acc)))))
        (iterate-base initial-base initial-acc)))

; returns the number of digits in `n`
(define (digit-count n)
    (let ([acc 1])
        (if (< n 10)
            acc
            (+ acc (digit-count (/ n 10))))))

; counts all numbers by raising to powers from 1 until the
; count of numbers for a given power is 0
(define (get-total)
    (let ([start-count 0]
          [start-power 1])
        (define (iterate-power power acc)
            (let ([partial-count (get-count-for-constant-power power)])
                (if (= 0 partial-count)
                    acc
                    (iterate-power (+ 1 power) (+ acc partial-count)))))
        (iterate-power start-power start-count)))

(get-total)
