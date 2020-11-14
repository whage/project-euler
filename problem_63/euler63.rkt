#lang racket

;(define (get-count-for-constant-base base)
;    (let ([initial-power 1]
;          [initial-acc 0])
;        (define (iterate-power power acc)
;            (if (>= base 10)
;                0
;                (if (>= (digit-count (expt base power)) power)
;                    (iterate-power (+ 1 power) (+ 1 acc))
;                    acc)))
;        (iterate-power initial-power initial-acc)))

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

(define (digit-count n)
    (let ([acc 1])
        (if (< n 10)
            acc
            (+ acc (digit-count (/ n 10))))))

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
