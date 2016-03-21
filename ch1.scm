;; 朴素的斐波那契数列递归算法
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib ( - n 2))))))

;; 迭代算法
(define (fib-it a b count)
  (cond ((= count 0) b)
	(else (fib-it (+ a b) a (- count 1)))))

(define (fib2 n) (fib-it 1 0 n))

;; 1.11
(define (f11 n)
  (cond ((< n 3) n)
  (else (+ (f11 (- n 1))
	   (* 2 (f11 (- n 2)))
	   (* 3 (f11 (- n 3)))))))

(define (f11-it a b c n)
  (cond ((< n 3) a)
	(else (f11-it (+ a (* 2 b) (* 3 c)) a b (- n 1)))))
(define (f112 n) (f11-it 2 1 0 n))

;; 1.12
(define (psc n k)
  (cond	((> k n) 0)
	((= n 0) 1)
	((= k 0) 1)
	(else (+ (psc (- n 1) (- k 1))  (psc (- n 1) k)))))

(psc 4 2)

;; 1.16
(define (expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (expt-iter  (* b b)  (/ n 2)  a))
	(else (expt-iter  b (- n 1) (* b a)))))

;; 1.17
(define (fast-mul a n)
  (cond ((= n 0) 0)
	((= n 1) a)
	((even? n) (fast-mul (+ a a) (/ n 2)))
	(else (+ a (fast-mul a (- n 1))))))
;; 1.18
(define (mul-iter b n a)
  (cond ((= n 0) a)
	((even? n) (mul-iter (+ b b) (/ n 2) a))
	(else (mul-iter b (- n 1) (+ a b)))))

(mul-iter 3 5 0)

