#lang racket

(require math/number-theory)
(require racket/trace)

; finds minimal x for a given D by iterating y
; very inefficient, minimal x for D=61 is more than 1.5 billion
(define (get-minimal-x-for-given-D-by-iterating-y D y)
    (if (perfect-square D)
        0
        (let* ([x-squared (+ (* D (* y y)) 1)]
               [x (perfect-square x-squared)])
            (if x
                x
                (get-minimal-x-for-given-D-by-iterating-y D (+ y 1))))))

;(trace get-minimal-x-for-given-D-by-iterating-y)
;(get-minimal-x-for-given-D-by-iterating-y 97 2)

;(define (get-minimal-x-for-given-D-by-iterating-x D x)
;    (if (perfect-square D)
;        0
;        (if (= 0 (modulo (- (* x x) 1) D))
;            (let* ([y (/ (- (* x x) 1) D)]
;                   [square-root (perfect-square y)])
;                (if square-root
;                    x
;                    (get-minimal-x-for-given-D-by-iterating-x D (+ x 1))))
;            (get-minimal-x-for-given-D-by-iterating-x D (+ x 1)))))

;(trace get-minimal-x-for-given-D-by-iterating-x)
;(get-minimal-x-for-given-D-by-iterating-x 97 2)

; iterates D from 2 -> 1000 calling the above function for finxing x for each D
(define (find-D-where-x-is-largest D max-x-so-far D-of-max-x)
    (let* ([current-x (get-minimal-x-for-given-D-by-iterating-y D 2)] ;TODO: constant argument "2" is ugly, it
           [new-D-of-max-x (if (> current-x max-x-so-far) D D-of-max-x)]
           [new-max-x (max max-x-so-far current-x)])
        ;new-max-x))
        (if (= D 1000)
            new-D-of-max-x
            (find-D-where-x-is-largest (+ 1 D) new-max-x new-D-of-max-x))))

(trace get-minimal-x-for-given-D-by-iterating-y)
(trace find-D-where-x-is-largest)
(find-D-where-x-is-largest 2 3 2)
