;;ch2.1
(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

;; 定义有理数并用gcd优化
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)  (/ d g))))
;; 分子
(define (number x) (car x))
;; 分母
(define (denom x) (cdr x))
;; print
(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))
;; test val->1/2
(print-rat (make-rat 2 -4))


;; ex 2.1
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((> (* n d) 0)
           (cons (/ (abs n) g)  (/ (abs d) g)))
          (else  (cons (- 0 (/ (abs n) g)) (/ (abs d) g))))))
;; ex 2.2
(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))
(define (make-segment x y) (cons x y))
(define (start-point x) (car x))
(define (end-point x) (cdr x))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (mid-point segment)
  (print-point (make-point
   (/ (+ (x-point (start-point segment)) (x-point (end-point segment))) 2)
   (/ (+ (y-point (start-point segment)) (y-point (end-point segment))) 2))))

(mid-point (make-segment (make-point 1.0 1.0) (make-point 4 5)))
;; value-> (2.5,3.)

;; ex 2.3
(define (double x) (* x x))
(define (make-rect p1 p2 p3)
  (cons (make-segment p1 p2) (make-segment p2 p3)))
(define (h-side rect) (car rect))
(define (v-side rect) (cdr rect))
(define (lenth seg)
  (let ((p1 (x-point seg))
        (p2 (y-point seg)))
  (sqrt
   (+
    (double (- (x-point p1) (x-point p2)))
    (double (- (y-point p1) (y-point p2)))))))
(define (rect-length rect)
  (* (+
      (lenth (h-side rect))
      (lenth (v-side rect)))
     2))
(define (rect-area rect)
  (*  (lenth (h-side rect))
      (lenth (v-side rect))))

(lenth (make-segment (make-point 1 1) (make-point 2 2))) ;Value: 1.4142135623730951

(rect-length
 (make-rect (make-point 1 1) (make-point 2 2) (make-point 0 2))) ;Value: 6.82842712474619

(rect-area
 (make-rect (make-point 1 1) (make-point 2 2) (make-point 0 2))) ;Value: 2.8284271247461903

;; ex 2.5
(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter  (* b b)  (/ n 2)  a))
        (else (expt-iter  b (- n 1) (* b a)))))
(define (expt a n) (expt-iter a n 1))

(define (cons-expt x y)
  (* (expt 2 x) (expt 3 y)))
(cons-expt 2 2)
(define (car-expt x)
  (define (iter a n)
    (cond ((= 0 (remainder a 2)) (iter (/ a 2) (+ n 1)))
          (else n)))
  (iter x 0))

(define (cdr-expt x)
  (define (iter a n)
    (cond ((= 0 (remainder a 3)) (iter (/ a 3) (+ n 1)))
          (else n)))
  (iter x 0))

(car-expt (cons-expt 5 6))
(cdr-expt (cons-expt 5 6))

;; ex 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f (n f) x))))

