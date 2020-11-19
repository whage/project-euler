#lang racket

(require math/number-theory)

(define (get-digits number acc)
    (if (= (quotient number 10) 0)
        (cons (modulo number 10) acc)
        (get-digits (quotient number 10) (cons (modulo number 10) acc))))

(define (get-sorted-digits number)
    (let ([acc (list)])
        (sort (get-digits number acc) <)))

(define (brute-force-totient n other acc)
		(if (= other 0)
			acc
			(if (coprime? n other)
				(totient n (- other 1) (+ 1 acc))
				(totient n (- other 1) acc))))

(define (totient-using-brute-force n)
	(if (= n 1)
		n
		(let ([other (- n 1)]
			  [acc 0])
			(brute-force-totient n other acc))))

;(totient-using-brute-force 10)

;(define (divide-while-modulo-is-zero n i)
;	(if (= (modulo n i) 0)
;		(divide-while-modulo-is-zero (/ n i) i)
;		n))

;(define (totient-using-prime-factorization n i result)
;	(if (> (* i i) n)
;		result
;		(if (= (modulo n i) 0)
;			)
;		(totient-using-prime-factorization n (+ i 1) (- result (/ result i)))))

(define (iterate i limit min-ratio min-so-far)
	(if (> i limit)
		min-so-far
		(let ([current-ratio (/ i (totient i))])
			(if (equal? (get-sorted-digits i) (get-sorted-digits (totient i)))
				(iterate (+ i 1) limit (min min-ratio current-ratio) (if (< current-ratio min-ratio)
					i
					min-so-far))
				(iterate (+ i 1) limit min-ratio min-so-far)))))

(define (find-min)
	(iterate 2 10000000 10 2)
	)

(find-min)