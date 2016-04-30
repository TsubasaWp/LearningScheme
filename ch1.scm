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

;; 1.19 
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))
		   (+ (* 2 p q) (* q q))
		   (/ count 2)))
	 (else (fib-iter (+ (* b q) (* a q) (* a p))
			 (+ (* b p) (* a q))
			 p
			 q
			 (- count 1)))))

;; 1.20
(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

(gcd 3 9)
;; ch 1.2.6
(define (expmod bas exp m)
  (cond ((= exp 0 ) 1)
	((even? exp)
	 (remainder (square (expmod bas (/ exp 2) m)) m))
	(else
	 (remainder (* bas (expmod bas (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= n 1) true)
	((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(fast-prime? 1 11)

(define (divisor-test a n)
  (cond ((= n 1) 1)
	((= n 0) 0)
	((and (fast-prime? a 10) (= (remainder n a) 0)) a)
	(else (divisor-test (+ a 1) n))))

(define (smallest-divisor n) (divisor-test 2 n))

(smallest-divisor 19999)

;;1.22
(define (smallest-divisor-sqre n)
  (let  ((t (real-time-clock)))
    (display (find-divisor-sqre n 2))
    (- (real-time-clock) t)))

(define (find-divisor-sqre n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? n test-divisor) test-divisor)
	(else (find-divisor-sqre n (+ test-divisor 1)))))

(define (divides? n test-divisor) 
  (= (remainder n test-divisor) 0))

(define (prime? n) (= n (smallest-divisor-sqre n)))

(define (search-for-prime n)
  (cond ((prime? n) 
	(display n))
	(else (cond ((even? n)
		     (search-for-prime (+ n 1)))
		    (else 
		     (search-for-prime (+ n 2)))))))

(define (start-search-prime n start-time)
  (search-for-prime n)
  (newline)
  (display (- (real-time-clock) start-time)))

(define (time-for-search-prime n)
  (start-search-prime n (real-time-clock)))

(time-for-search-prime 10000000000)

;;1.23
(define (smallest-divisor-fast n)
  (let  ((t (real-time-clock)))
    (display (find-divisor-fast n 2))
    (- (real-time-clock) t)))

(define (find-divisor-fast n test)
  (cond ((> (square test) n) n)
	((divides? n test) test)
	(else (cond ((= test 2) (find-divisor-fast n 3))
		    (else (find-divisor-fast n (+ test 2)))))))

(define (next n)
  (cond ((even? n) (+ n 1))
	(else (+ n 2))))

;;挑选几个素数1009,10009,20029
(smallest-divisor-fast 10099081)
(smallest-divisor-sqre 10099081)
(smallest-divisor-fast 100180081)
(smallest-divisor-sqre 100180081)
(smallest-divisor-fast (* 20029 20029))
(smallest-divisor-sqre (* 20029 20029))

;; 测试remainder的速度
(define (remainder-it n cont)
  (remainder 1000 n)
  (cond ((= cont 0) 0)
	(else (remainder-it n (- cont 1)))))

(define (remainder-time n)
  (let ((t (real-time-clock)))
    (remainder-it n 1000000)
    (- (real-time-clock) t)))

(remainder-time 7)

;;1.27
(fast-prime? 561 50)
(fast-prime? 1105 50)
(fast-prime? 1729 50)
(fast-prime? 2465 50)
(fast-prime? 2821 50)
(fast-prime? 6601 50)

;;1.28
(define (expmod bas exp m)
  (cond ((= exp 0 ) 1)
	((even? exp)
	 (remainder (square (expmod bas (/ exp 2) m)) m))
	(else
	 (remainder (* bas (expmod bas (- exp 1) m)) m))))

(define (miller-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= n 1) true)
	((= times 0) true)
	((miller-test n) (fast-prime? n (- times 1)))
	(else false)))

(fast-prime? 6601 30)

;; ch1.3
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (cub n) (* n n n))
(define (add n) (+ n 1))
(sum cub 1 add 9)

;; 1.30
(define (sum term a next b)
  (define (iter n result)
    (if (> n b) 
	result
	(iter (next n) (+ result (term n)))))
  (iter a 0))

(define (next a) (+ 1 a))
(define (sqr a) (* a a))
(sum sqr 1 next 3)

;; 1.31
;; 递归
(define (product a b term)
 (if (> a b)
       1
       (* (term a) (product (+ a 1) b term))))

;; 迭代
(define (product-iter a b next)
  (define (iter n result)
    (if (> n b) 
	result
	(iter (+ n 1) (* result (term n)))))
  (iter a 0))

;; 分子
(define (term-numer a)
  (cond ((odd? a) (+ a 1))
	(else (+ a 2))))
;; 分母
(define (term-deno a)
  (cond ((even? a) (+ a 1))
	(else (+ a 2))))

;; pi/4
(define (quarter-pi n)
  (/ (product 1 n term-numer) (product 1 n term-deno)))

(quarter-pi 10)
;; 1.32  
(define (accumulate combinder null-value term a next b)
  (if (> a b) null-value
      (combinder (term a) (accumulate combinder null-value term (next a) next b))))
(accumulate + 0 (lambda (a) a) 1 (lambda (a) (+ a 1)) 10)
(accumulate * 1 (lambda (a) a) 1 (lambda (a) (+ a 1)) 10)
;; 1.33
(define (filtered-accumulate filter combinder null-value term a next b)
  (cond ((> a b) null-value)
	((filter a) 
	 (combinder (term a) 
	      (filtered-accumulate filter combinder null-value term (next a) next b)))
	(else 
	 (combinder null-value
	 (filtered-accumulate filter combinder null-value term (next a) next b)))))
(filtered-accumulate prime? + 0 (lambda (a) a) 1 (lambda (a) (+ a 1)) 10)
;; ch 1.3.3
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
(fixed-point (lambda (x) (+ 1 (/ 1 x)))  1.0)(quarter-pi 10)
;; 1.32  
(define (accumulate combinder null-value term a next b)
  (if (> a b) null-value
      (combinder (term a) (accumulate combinder null-value term (next a) next b))))
(accumulate + 0 (lambda (a) a) 1 (lambda (a) (+ a 1)) 10)
(accumulate * 1 (lambda (a) a) 1 (lambda (a) (+ a 1)) 10)
;; 1.33
(define (filtered-accumulate filter combinder null-value term a next b)
  (cond ((> a b) null-value)
	((filter a) 
	 (combinder (term a) 
	      (filtered-accumulate filter combinder null-value term (next a) next b)))
	(else 
	 (combinder null-value
	 (filtered-accumulate filter combinder null-value term (next a) next b)))))
(filtered-accumulate prime? + 0 (lambda (a) a) 1 (lambda (a) (+ a 1)) 10)
;; ch 1.3.3
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; 1.36
(define (fixed-point-log f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (average n) (/ n 2))
(fixed-point-log  (lambda (x) (average (+ x (/ (log 1000) (log x))))) 2)
(fixed-point-log  (lambda (x)  (/ (log 1000) (log x))) 2)
;; 1.37
;; 递归
(define (cont-frac n d k it)
  (cond ((= it k) 0)
	(else (/ (n it) (+ (d it) (cont-frac n d k (+ it 1)))))))

(define (gold k)
    (+ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k 0)))

(gold 11)

;; 迭代
(define (cont-frac-it n d k result)
  (cond ((= k -1) result)
	(else (cont-frac-it n d (- k 1) (/ (n k) (+ (d k) result))))))

(define (gold k)
    (+ 1 (cont-frac-it (lambda (i) 1.0) (lambda (i) 1.0) k 0)))

(gold 11)

;; 1.38
(define (ora i)
    (cond ((= 0 (remainder (- i 1) 3))
	   (* 2 (/ (+ i 2) 3)))
	  (else 1.0)))

(define (de-frac k)
  (cont-frac-it
  (lambda (i) 1.0)
  ora
  k
  0))

(define (e k)
  (+ 2 (de-frac k)))

(e 10)
;; 1.39
(define (tan-cf x k)
  (define (N-tan i) 
    (if (= i 1)
	x
	(- (square x))))
  (define (D-tan i) (- (* 2 i) 1))
  (cont-frac N-tan D-tan k 1))

(tan-cf 1.0 10)
(tan 1)

;;ch1.3.4
(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
((average-damp square) 10)

(define (squrt x)
  (fixed-point (average-damp (lambda (a) (/ x a))) 1.0))
(squrt 16)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
(cube-root 8.0)

;; 导数
(define dx 0.00001)
(define (driv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((driv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g ) guess))

(define (squr x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))
(squr 10)

;;1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 3 2 1) 1)
;;1.41
(define (double f)
  (lambda (x)
    (f (f x))))
;; test
((double (lambda (x) (square x))) 2)
(define (inc x) (+ x 1))
(((double (double double)) inc ) 0) 
;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)
;;1.43
(define (repeated f k)
  (define (it g k n)
    (if (= n (- k 1)) 
	(lambda (x) (g x))
	(it (lambda (x) (f (g x))) k (+ n 1))))
  (it f k 0))

((repeated square 2) 5)
;;1.44
(define (smooth f)
  (lambda (x) 
    (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

(define (smooth-k f k)
  ((repeated smooth k) f))

((smooth-k sin 9) 2)
;;1.45
(define (expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (expt-iter  (* b b)  (/ n 2)  a))
	(else (expt-iter  b (- n 1) (* b a)))))

(define (root n x)
  (lambda (y) (/ x (expt-iter y n 1))))

(define (repeated-damp f k)
  ((repeated average-damp k) f))

(define (squrt-n n x damp-cnt)
  (fixed-point-log (repeated-damp (root n x) damp-cnt) 1))

(squrt-n 2 9.0 2)
;;1.46
(define (iterative-improve good-enough? f)
  (lambda (x)
    (define (try guess)
      (let ((next (f guess)))
	(if (good-enough? guess next)
	    next
	    (try next))))
    (try x)))

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f guess)
  ((iterative-improve close-enough? f) guess))

(define (sqrt n guess)
    ((iterative-improve 
      close-enough? (lambda (x) (/ (+ x (/ n x)) 2))) guess))


(define (squrt-n n x damp-cnt)
  (fixed-point (repeated-damp (root n x) damp-cnt) 1))















