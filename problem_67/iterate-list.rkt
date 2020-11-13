#lang racket

(define (map-with-index fn lst idx)
    (if (empty? lst)
        empty
        (cons (fn idx) (map-with-index fn (cdr lst) (+ idx 1)))))

(define (my-map fn lst)
    (if (empty? lst)
        empty
        (cons (fn (first lst)) (my-map fn (cdr lst)))))

;(my-map (lambda (n) (* n n))'(1 2 3 4 5 6))

(define (my-test lst)
    (let ([other-list (list 7 4)])
        (map-with-index (lambda (idx)
            (if (= idx 0)
                (+ (first other-list) (first lst))
                (max (+ (list-ref lst idx) (if (= idx (length other-list)) 0 (list-ref other-list idx)))
                     (+ (list-ref lst idx) (list-ref other-list (- idx 1)))))) lst 0)))


(my-test (list 2 4 6))
