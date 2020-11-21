#lang racket

(require math/number-theory)

; returns the digits of given number as a list
(define (get-digits number acc)
    (if (= (quotient number 10) 0)
        (cons (modulo number 10) acc)
        (get-digits (quotient number 10) (cons (modulo number 10) acc))))

; sorts the digits of given number
(define (get-sorted-digits number)
    (let ([acc (list)])
        (sort (get-digits number acc) <)))

; a brute force algorithm for finding totient of n
; very inefficient but runs correctly
(define (brute-force-totient n other acc)
        (if (= other 0)
            acc
            (if (coprime? n other)
                (totient n (- other 1) (+ 1 acc)) ; if the two numbers are coprime then add 1 to acc and move forward
                (totient n (- other 1) acc)))) ; if not, then just move forward without incrementing acc

; wrapper fucntion for brute force totient algorithm
(define (totient-using-brute-force n)
    (if (= n 1)
        n
        (let ([other (- n 1)]
              [acc 0])
            (brute-force-totient n other acc))))

;(totient-using-brute-force 10)

;(define (divide-while-modulo-is-zero n i)
;   (if (= (modulo n i) 0)
;       (divide-while-modulo-is-zero (/ n i) i)
;       n))

;(define (totient-using-prime-factorization n i result)
;   (if (> (* i i) n)
;       result
;       (if (= (modulo n i) 0)
;           )
;       (totient-using-prime-factorization n (+ i 1) (- result (/ result i)))))

; checks every number from i -> limit while keeping track of the min-ratio
(define (iterate i limit min-ratio min-so-far)
    (if (> i limit)
        min-so-far
        (let ([current-ratio (/ i (totient i))])
            (if (equal? (get-sorted-digits i) (get-sorted-digits (totient i)))
                (iterate (+ i 1) limit (min min-ratio current-ratio) (if (< current-ratio min-ratio)
                    i
                    min-so-far))
                (iterate (+ i 1) limit min-ratio min-so-far)))))

; wrapper function
(define (find-min)
    (iterate 2 10000000 10 2)
    )

; runs for a very long time!
(find-min)
