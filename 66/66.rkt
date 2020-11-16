#lang racket

(require math/number-theory)
(require racket/trace)

(define (get-minimal-x-for-given-D D y)
	(if (perfect-square D)
		0
		(let* ([term (+ (* D (* y y)) 1)]
			   [square-root (perfect-square term)])
			(if square-root
				square-root
				(get-minimal-x-for-given-D D (+ y 1))))))

;(trace get-minimal-x-for-given-D)
(get-minimal-x-for-given-D 60 2)


(define (find-D-where-x-is-largest D max-x-so-far D-of-max-x)
	(let* ([current-x (get-minimal-x-for-given-D D 2)] ;TODO: constant argument "2" is ugly, it
	       [new-D-of-max-x (if (> current-x max-x-so-far) D D-of-max-x)]
	       [new-max-x (max max-x-so-far current-x)])
		;new-max-x))
		(if (= D 100)
			new-D-of-max-x
			(find-D-where-x-is-largest (+ 1 D) new-max-x new-D-of-max-x))))

;(trace find-D-where-x-is-largest)
;(find-D-where-x-is-largest 2 0 2)
