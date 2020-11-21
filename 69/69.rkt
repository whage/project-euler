#lang racket

(require racket/trace)

; same as `map` but supplies the current index to the callback too
(define (map-with-index fn lst idx)
    (if (empty? lst)
        empty
        (cons (fn idx) (map-with-index fn (cdr lst) (+ idx 1)))))

; creates a list of `n` #t elements (for sieve of eratosthenes)
(define (create-bool-list n)
    (let ([l (list #t)])
        (if (= n 1)
            l
            (cons #t (create-bool-list (- n 1))))))

;(create-bool-list 5)

; implements inner loop of sieve of eratosthenes
(define (flip-at-every-jth-step idx step-size items)
    (if (< idx (length items))
        (flip-at-every-jth-step (+ idx step-size) step-size (list-set items idx #f))
        items))

;(flip-at-every-jth-step 4 2 (create-bool-list 20))

; implements main algorithm for sieve of eratosthenes
(define (use-sieve-of-eratosthenes idx items)
    (if (<= idx (sqrt (length items)))
        (if (list-ref items idx)
            (use-sieve-of-eratosthenes (+ idx 1) (flip-at-every-jth-step (* idx idx) idx items))
            (use-sieve-of-eratosthenes (+ idx 1) items))
        items))

; generates `n` primes using sieve of eratosthenes
(define (get-primes-up-to n)
    (let ([bool-list (use-sieve-of-eratosthenes 2 (create-bool-list n))])
        (filter (lambda (n) (> n 1))
            (map-with-index (lambda (idx)
                (if (list-ref bool-list idx)
                    idx
                    0)) bool-list 0))))

;(trace use-sieve-of-eratosthenes)
;(use-sieve-of-eratosthenes 2 (create-bool-list 200))

; gets a list of the first 200 primes
(define first-200-primes (get-primes-up-to 200))

; multiplies given items until the product reaches 1000000
(define (multiply-items items acc)
    (if (<= (* (car items) acc) 1000000)
        (multiply-items (cdr items) (* (car items) acc))
        acc))

(multiply-items first-200-primes 1)
