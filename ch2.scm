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

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda(x) x) x))))
(lambda (f) (lambda (x) (f x)))
(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
; so, we can define the one
(define one (lambda (f) (lambda (x) (f x))))
; so, we can define the two
(define two (lambda (f) (lambda (x) (f (f x)))))
;规律:one的定义中应用了一次f, two的定义中应用了2次f, 所以这类过程可以理解为用'应用f的次数'来表示数字.
(define (add a b)
  (lambda(f) (lambda(x) ((a f) ((b f) x)))))

;; test
(add one two)
(lambda(f) (lambda(x) ((lambda(x) (f x)) ((lambda(x) (f (f x))) x))))
(lambda(f) (lambda(x) ((lambda(x) (f x)) (f (f x)))))
(lambda(f) (lambda(x) (f (f (f x))))) ; three

;; 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;;2.8
(define (sub-interval a b)
  (make-interval (- (upper-bound a) (upper-bound b))
                 (- (lower-bound a) (lower-bound b))))

;; 2.10
(define (valid-interval x)
  (if (< (* (upper-bound x) (lower-bound x)) 0) false true))

(check-interval (make-interval -2 2))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (if (and (valid-interval x) (valid-interval y))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      (display "error:invalid param")))

(div-interval (make-interval 1 2) (make-interval 2 2))

;; 2.11
(define (is-cross-zero x)
  (if (< (* (upper-bound x) (lower-bound x)) 0) true false))
(define (is-positive x)
  (if (>= (lower-bound x) 0) true false))
(define (is-negative x)
  (if (< (upper-bound x) 0) true false))

(is-positive (make-interval -1 1))
(is-negative (make-interval -1 -1))

(define (fast-mul-interval x y)
  (cond
   ((and (is-positive x) (is-positive y))
    (make-interval
     (* (lower-bound x) (lower-bound y))
     (* (upper-bound x) (upper-bound y))))
   ((and (is-positive x) (is-negative y))
    (make-interval
     (* (upper-bound x) (upper-bound y))
     (* (lower-bound x) (lower-bound y))))
   ((and (is-positive x) (is-cross-zero y))
    (make-interval
     (* (upper-bound x) (lower-bound y))
     (* (upper-bound x) (upper-bound y))))
   ((and (is-negative x) (is-positive y))
    (make-interval
     (* (lower-bound x) (upper-bound y))
     (* (upper-bound x) (lower-bound y))))
   ((and (is-negative x) (is-negative y))
    (make-interval
     (* (upper-bound x) (upper-bound y))
     (* (lower-bound x) (lower-bound y))))
   ((and (is-negative x) (is-cross-zero y))
    (make-interval
     (* (lower-bound x) (upper-bound y))
     (* (lower-bound x) (lower-bound y))))
   ((and (is-cross-zero x) (is-positive y))
    (make-interval
     (* (lower-bound x) (upper-bound y))
     (* (upper-bound x) (upper-bound y))))
   ((and (is-cross-zero x) (is-negative y))
   (make-interval
    (* (upper-bound x) (lower-bound y))
    (* (lower-bound x) (lower-bound y))))
  ((and (is-cross-zero x) (is-cross-zero y))
   (make-interval
    (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
    (max (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)) )))))


(fast-mul-interval (make-interval 1 2) (make-interval 1 2))
(mul-interval (make-interval 1 2) (make-interval 1 2))
;; Value : (1 . 4)

(fast-mul-interval (make-interval 1 2) (make-interval -1 -2))
(mul-interval (make-interval 1 2) (make-interval -1 -2))
;; Value : (-4 .-1)

(fast-mul-interval (make-interval 1 2) (make-interval -1 2))
(mul-interval (make-interval 1 2) (make-interval -1 2))
;; Value : (-2, 4)

(fast-mul-interval (make-interval -2 -1) (make-interval 1 2))
(mul-interval (make-interval -2 -1) (make-interval 1 2))
;; Value : (-4, -1)

(fast-mul-interval (make-interval -2 -1) (make-interval -2 -1))
(mul-interval (make-interval -2 -1) (make-interval -2 -1))
;; Value : (1, 4)

(fast-mul-interval (make-interval -2 -1) (make-interval -1 2))
(mul-interval (make-interval -2 -1) (make-interval -1 2))
;; Value : (-4, 2)

(fast-mul-interval (make-interval -2 1) (make-interval 1 2))
(mul-interval (make-interval -2 1) (make-interval 1 2))
;; Value : (-4, 2)

(fast-mul-interval (make-interval -2 1) (make-interval -2 -1))
(mul-interval (make-interval -2 1) (make-interval -2 -1))
;; Value : (-2, 4)

(fast-mul-interval (make-interval -2 1) (make-interval -5 2))
(mul-interval (make-interval -2 1) (make-interval -5 2))
;; Value : (-5, 10)

;; 2.12
(define (make-center-percent center percent)
  (make-interval (- center (* center percent))
                 (+ center (* center percent))))
(define (percent range)
  (let ((width (/ (- (upper-bound range) (lower-bound range)) 2))
        (mid (/ (+ (upper-bound range) (lower-bound range)) 2)))
    (/ width mid)))
;; 2.13
(define (percent-mul r1 r2)
  (+ (percent r1) (percent r2)))

(percent (mul-interval (make-interval 9.99 10.01) (make-interval 9.99 10.01)))
(percent-mul (make-interval 9.99 10.01) (make-interval 9.99 10.01))

(define (add-interval r1 r2)
  (make-interval (+ (lower-bound r1) (lower-bound r2))
                 (+ (upper-bound r1) (upper-bound r2))))

(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
;; 2.14
(let ((r1 (make-interval 1.0 2))
      (r2 (make-interval 1.0 2))
      (one (make-interval 1.0 1.0)))

  (percent r1)  ; .33
  (percent (mul-interval r1 r2)) ; .6 放大2倍
  (percent (div-interval r1 r2)) ; .6 放大2倍
  (percent (add-interval r1 r2)) ; .33 不变
  (percent (div-interval one r1)) ; .33 不变
  (percent (part1 r1 r2)) ; .77 运算过程中,乘法放大2倍, 用放大的乘积除以和之后从0.6放大到0.77
  (percent (part2 r1 r2)) ; .33 全是除以1的操作.不变
  )


;; 2.15
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (better-mul-interval r1 r2)
  (let ((one (make-interval 1 1)))
        (mul-interval (add-interval r1 r2)
                 (div-interval one
                               (add-interval (div-interval one r1)
                                             (div-interval one r2))))))

(define (better-div-interval r1 r2)
  (let ((one (make-interval 1 1)))
    (better-mul-interval r1  (div-interval one r2))))


(let ((r1 (make-interval 1.0 2))
      (r2 (make-interval 1.0 2))
      (one (make-interval 1.0 1.0)))
  (mul-interval r1 r2)
  (better-mul-interval r1 r2)
  )

;; ch 2.2
(define (list-ref item n)
  (if (= n 0)
      (car item)
      (list-ref (cdr item) (- n 1))))

(define squares (list 1 4 9 16 64))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(length squares)

(define (append list1 list2)
  (cond ((null? list1)
         list2)
        ((null? list2)
         list1)
        ((and (not (pair? list1)) (not (pair? list2)))
         (list list1 list2))
        ((not (pair? list1))
         (cons list1 list2))
        (else
         (cons (car list1) (append (cdr list1) list2)))))
;; 2.17
(define (last-pair items)
  (if (= 1 (length items))
      (list (car items))
      (last-pair (cdr items))))
;; 2.18
(define (reverse items)
  (if (= 1 (length items))
      items
      (append (reverse (cdr items)) (list (car items)))))
(reverse squares)

;; 2.19
(restart 1)
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (first-denomination coin-values) (car coin-values))
(define (no-more? coin-values) (if (null? coin-values) true false))
(define (cc amount coin-values)
  (cond ((= amount 0 ) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(cc 20 (reverse us-coins))
(cc 20  us-coins)

;; 2.20
(define (filter items f)
  (cond
   ((null? items) (list))
   ((f (car items))
    (append (list (car items)) (filter (cdr items) f)))
   (else
    (append (list) (filter (cdr items) f)))))


(define (same-party x y . z)
  (let ((param-list
         (append (list x) (append (list y) z))))
    (if
     (odd? x)
     (filter param-list odd?)
     (filter param-list even?))))

(same-party 2 1 2 3 4 5 6 7 8)

;;2.2
(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 -2 -3))

(cons 1 (cons 1 (cons 1 (list))))

;;ex2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;;ex2.22
(define (for-each proc items)
  (if (null? items)
      '()
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(for-each display '(1 2 3 4))

;; 2.2.2
(define my-tree (cons (list 1 2 3 4) (list 3 4)))
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves my-tree)

;; 2.24
(count-leaves (list 1 (list 2 (list 3 4) 5)))
;;2.25
(define mt1 (list 1 3 (list 5 7) 9))
(define mt2 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cdr (car (cdr (cdr mt1))))
(cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr mt2)))))))))))
;;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)


;; 2.27
(define (deep-revers tree)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (list tree))
   ((pair? (car tree))
    (append (deep-revers (cdr tree)) (list (deep-revers (car tree)))))
   (else
    (append (deep-revers (cdr tree)) (deep-revers (car tree))))))

(define x (list (list 1 2 3 4) (list 5 6)))

(reverse x)
(deep-revers x)
;;2.28
(define (fringe tree)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (list tree))
   (else
    (append (fringe (car tree))
            (fringe (cdr tree)))
    )))

(fringe (list x x))
;; 2.29
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
   (cdr mobile))

(define (branch-length branch)
  (cond
   ((null? branch) 0)
   ((not (pair? branch)) 0)
   (else
    (+ (car branch) (branch-length (right-branch branch))))))

(define (branch-weight branch)
  (cond
   ((null? branch) 0)
   ((not (pair? branch)) branch)
   (else
    (branch-weight (right-branch branch)))))

(define (total-weight mobile)
  (+
   (branch-length (left-branch mobile))
   (branch-length (right-branch mobile))))

(define (is-balance mobile)
  (=
   (* (branch-weight (left-branch mobile)) (branch-length (left-branch mobile)))
   (* (branch-weight (left-branch mobile)) (branch-length (right-branch mobile)))))


(define l (make-branch 5 (list 1 1)))
(define r (make-branch 2 (list 4 1)))
(define m (make-mobile l r))

(total-weight m)
(is-balance m)

;; ch2.2
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; 2.30
(define (map-tree tree f)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree sub-tree f)
             (f sub-tree)))
       tree))
(map-tree (list 1 2 (list 3 4)) square)
;;2.32
(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (subsets s)
  (if (null? s)
      ()
      (let ((reset (subsets (cdr s))))
        (append reset
                (cons
                 (car s)
                 (map (lambda (sub) (append (car s) sub)) reset))))))

(subsets (list 1 2 3 4))

(cons 1 (map (lambda (sub) (append 4 sub)) (list 0 1 (list 2 3))))

(restart 1)

;; 2.2.3
(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
;; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
;; 2.34
(define (horner-eval x sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              sequence))

(horner-eval 2 (list 1 3 0 5 0 1));Value: 79
;;2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (x) (length (enumerate-tree x))) t)))
(count-leaves (list 1 2 (list 2 3 4))) ; Value:5
;;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 1 2 3)))
;;2.37
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map
   (lambda (x) (dot-product x v))
   m))
(define (transpose m)
  (accumulate-n (lambda (x y) (cons x y)) () m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (col) (matrix-*-vector m col)) cols)))

(define mv (list 1 2 3))
(define mm (list (list 1 2 3) (list 4 5 6) (list 4 5 6)))
(dot-product mv mv);Value: 14
(matrix-*-vector mm mv);Value 269: (14 32 32)
(transpose mm);Value 266: ((1 4) (2 5) (3 6))
(matrix-*-matrix mm mm);Value 268: ((21 48 48) (27 63 63) (33 78 78))
;;2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(fold-right / 1 (list 1 2 3)) ;;Value: 3/2
(fold-left / 1 (list 1 2 3))  ;;Value: 1/6
(fold-right list () (list 1 2 3));Value 270: (1 (2 (3 ())))
(fold-left list () (list 1 2 3));Value 271: (((() 1) 2) 3)
;;2.39
(restart 1)
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) ()  sequence))
(reverse (list 1 2 3))
(cons () 1)

;; ch2.3.2
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
;; map & flatmap
(map (lambda (x) (append x (list 9))) (list 1 2 3 4))
(flatmap (lambda (x) (append x (list 9))) (list 1 2 3 4))


