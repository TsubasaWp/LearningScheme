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
(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (prime? x) (if (= (remainder x 2) 1) #t #f))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair)))) ;; cadr
(prime-sum? (list 2 2))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)
                ))))
(prime-sum-pairs 3);=> ((2 1 3) (3 2 5))
;; 全排列
(define (permutations s)
  (if (null? s)
      (list (list))
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
;;

(define (team2 a b)
  (cons (a b)))

(define (team-div team-lst)
  (cond ((= 2 (length team-lst))
         team-lst)
        (else
         (append
         (paral-team team-lst)
         (cross-team team-lst)))))

(team-div (list 1 2 3 4))

(define (paral-team team-lst)
  (list
  (team-div (split-left team-lst))
  (team-div (split-right team-lst))))

(define (cross-team team-lst)
  (list
  (team-div (cross1 team-lst))
  (team-div (cross2 team-lst))))



(define (split-left lst)
  (let ((len (/ (length lst) 2)))
    (define (iter rest n)
      (if (= n 0)
          (list)
          (cons (car rest) (iter (cdr rest) (- n 1)))))
    (iter lst len)))

(define (split-right lst)
  (let ((len (length lst)))
    (define (iter rest n)
      (cond ((= n 0) (list))
            ((> n (/ len 2)) (append (list) (iter (cdr rest) (- n 1))))
            (else (cons (car rest) (iter (cdr rest) (- n 1))))))
    (iter lst len)))

(split-left (list 1 2 3 4 5 6 7 8))
(split-right (list 1 2 3 4 5 6 7 8))

(define (cross1 lst)
  (let ((len (length lst)))
    (define (iter rest n)
      (cond ((= n 0) (list))
            ((even? n) (append (list) (iter (cdr rest) (- n 1))))
            (else (cons (car rest) (iter (cdr rest) (- n 1))))))
    (iter lst len)))

(define (cross2 lst)
  (let ((len (length lst)))
    (define (iter rest n)
      (cond ((= n 0) (list))
            ((odd? n) (append (list) (iter (cdr rest) (- n 1))))
            (else (cons (car rest) (iter (cdr rest) (- n 1))))))
    (iter lst len)))

(cross1 (list 1 2 3 4 5 6 7 8))
(cross2 (list 1 2 3 4 5 6 7 8))

;; 2.42
(define (unique? sequence k)
  (if (null? sequence)
      #t
      (if (= k (car sequence))
          #f
          (unique? (cdr sequence) k))))
(define empty-board (list))
(define (queens board-size)
  (define (queen-cols k)
    (if (= 0 k)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
;; 每种摆法是一个排列
;; adjoin
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
;; safe

(define (safe? k positions)
  (define (safe-iter x rest distance)
    (cond ((null? rest) #t)
          ((= x (car rest)) #f)
          ((= distance (abs (- x (car rest)))) #f)
          (else (safe-iter x (cdr rest) (+ distance 1)))))
  (safe-iter (car positions) (cdr positions) 1))

(queens 4)
(restart 1)
;; other usage
(define (adjoin-position new-row k rest)
  (append rest (list (cons k new-row))))
(define (safe? k positions)
  (define (safe-iter pair rest)
    (cond ((null? rest)
           (if (= (car pair) (cdr pair)) #f #t))
          ((= (car pair) (cdr pair)) #f)
          ((equal-pair pair (car rest)) #f)
          (else (safe-iter pair (cdr rest)))))
  (safe-iter (car positions) (cdr positions)))

(define (print-list seq)
  (if (null? seq) (display "")
      (begin
        (newline)
        (display (car seq))
        (print-list (cdr seq)))))

(print-list (queens 4))
;;ch 2.2.4
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (up painter (right smaller smaller)))))
;;2.45
(define (split t1 t2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (painter (- n 1))))
          (let ((part (t2 smaller smaller)))
            (t1 painter part))))))
;;2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect rect) (car rect))
(define (ycor-vect rect) (cdr rect))
(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))
;;2.47
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))
;;2.48
(define (make-segment v1 v2) (list v1 v2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cadr seg))
;;2.49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
(define (make-frame r1 r2 r3) (list r1 r2 r3))

(define test-frame
  (make-frame
   (make-vect 0 0)
   (make-vect 0 1)
   (make-vect 1 0)))

(define (top-left frame) (add-vect (origin-frame frame) (edge1-frame frame)))
(define (top-right frame) (add-vect
                           (edge2-frame frame)
                           (add-vect (origin-frame frame) (edge1-frame frame))))
(define (bottom-left frame) (origin-frame frame))
(define (bottom-right frame) (add-vect (origin-frame frame) (edge2-frame frame)))

(define (left-mid frame) (add-vect (origin-frame frame)
                                   (scale-vect 0.5 (edge1-frame frame))))
(define (top-mid frame) (add-vect
                         (scale-vect 0.5 (edge2-frame frame))
                         (add-vect (origin-frame frame)
                                   (edge1-frame frame))))
(define (bottom-mid frame) (add-vect (origin-frame frame)
                                     (scale-vect 0.5 (edge2-frame frame))))
(define (right-mid frame) (add-vect
                         (edge2-frame frame)
                         (add-vect (origin-frame frame)
                                   (scale-vect 0.5 (edge1-frame frame)))))

(define (top frame)  (make-segment (top-left frame) (top-right frame)))
(define (left frame) (make-segment (top-left frame) (bottom-left frame)))
(define (right frame) (make-segment (top-right frame) (bottom-right frame)))
(define (bottom frame) (make-segment (bottom-left frame) (bottom-right frame)))

(define (painter-a f)
  (segments->painter
   (list (left f) (top f) (rignt f) (bottom f))))

(define (painter-b f)
  (segments->painter
   (list
    (make-segment (bottom-left f) (top-right f))
    (make-segment (bottom-right f) (top-left f)))))

(define (painter-c f)
  (segments->paiter
   (list
    (make-segment (left-mid f) (top-mid f))
    (make-segment (top-mid f) (right-mid f))
    (make-segment (right-mid f) (bottom-mid f))
    (make-segment (bottom-mid f) (left-mid f)))))

;; 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (flip-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (flip-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter painter1
                              split-point
                              (make-vect 1 0.5)
                              (make-vect 0 1)))
          (paint-below
           (transform-painter painter2
                              (make-vect 0 0)
                              (make-vect 1 0)
                              (make-vect 0 0.5))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below painter1 painter2)
  (lambda (frame)
    ((flip-horiz
      (rotate90
       (beside
        (rotate270
         (flip-horiz painter1))
        (rotate270
         (flip-horiz painter2)))))
     frame)))

;; 2.52
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))
(define (square-limit painter n)
  (let ((squarter (corner-split (flip-horiz painter) n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;; 2.53
(define (menq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (menq item (cdr x)))))
(list 'a 'b 'c);Value 468: (a b c)
(list (list 'george));Value 469: ((george))
(cdr '((x1 x2) (y1 y2)));Value 473: ((y1 y2))
(cadr '((x1 x2) (y1 y2)));(y1 y2)
(pair? (car '(a short list)));#f
(menq 'red '((red shoes) (blue socks)));#f
(menq 'red '(red shoes blue socks));Value 475: (red shoes blue socks)
;; 2.54
(define (equal? x y)
  (cond ((and (not (pair? x)) (not (pair? y))) (eq? x y))
        ((and (pair? x) (pair? y))
         (and (eq? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        (else false)))
;;2.55
(car '(quote a))
;;ch2.3.2
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- ERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;; ex2.5
(define (make-exponentiation a1 a2) (list '** a1 a2))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- ERIV" exp))))
(deriv '(** x 3) 'x);Value 490: (* 3 (** x 2))
;;2.57
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (cond
          ((not (pair? a2)) (cons '+ (append a1 (list a2))))
          (else (cons '+ (append a1 a2)))))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (cond
          ((not (pair? m2)) (cons '* (append m1 (list m2))))
          (else (cons '* (append m1 m2)))))))

(define (augend s) (caddr s))

(define (multiplicand p) (caddr p))

(deriv '(* (* x y) (+ x 3)) 'x)
;; 2.58
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (cond
          ((not (pair? a2)) (append a1 (append (list '+) (list a2))))
          (else (append a1 (append (list '+) a2)))))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (cond
          ((not (pair? m2)) (append m1 (append (list '*) (list m2))))
          (else (append m1 (append (list '*) m2)))))))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(deriv '(x + (x * y)) 'x)
;;ch2.3.3
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; 2.59
(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union (cdr set1) set2))
        (else (cons (car set1) (union (cdr set1) set2)))))

(define s1 (list 1 2 3))
(define s2 (list 2 3 4))
(intersection-set s1 s2)
(union s1 s2)
;;2.60
(define (element-of-set? x set)
  (define (iter x set count)
    (cond ((null? set) count)
          ((equal? x (car set))
                   (iter x (cdr set) (+ count 1)))
          (else (iter x (cdr set) count))))
  (iter x set 0))

(define (adjoin-set x set) (cons x set))

(define (cons-n x set n)
  (cond ((= n 0) set)
        (else (cons-n x (cons x set) (- n 1)))))

(define (intersection-set set1 set2)
  (define (iter set1 set2 result)
    (display result)
    (cond ((or (null? set1) (null? set2)) result)
          ((> (element-of-set? (car set1) result) 0)
           (iter (cdr set1) set2 result))
          (else (iter (cdr set1) set2
                      (cons-n
                       (car set1)
                       result
                       (min (element-of-set? (car set1) set1)
                            (element-of-set? (car set1) set2)))))))
  (iter set1 set2 ()))

(define (union set1 set2)
  (define (iter set1 set2 result)
    (cond ((null? set1) result)
          ((null? set2) set1)
          ((> (element-of-set? (car set1) result)
              (element-of-set? (car set1) set1))
           (iter (cdr set1) set2 result))
          (else (iter (cdr set1) set2
                      (cons-n
                       (car set1)
                       result
                       (- (element-of-set? (car set1) set1)
                            (element-of-set? (car set1) result)))))))
  (iter set1 set2 set2))

(union '(a a b d d) '(a c d e f))
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(element-of-set 3 (list  2 3 4)) ;; Value: #t
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      ()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(intersection-set '(1 2 2 4) '(2 2 3 4))
;;2.61
(define (adjoin-set x set)
  (define (iter x set1 set2)
    (cond ((= x (car set2)) (append set1 set2))
          ((< x (car set2)) (append set1 (append (list x (car set2)) (cdr set2))))
          (else (iter x (append set1 (list (car set2))) (cdr set2)))))
  (iter x () set))

(adjoin-set 5 '(1 2 3 6))
;;2.62

(define (union-set set1 set2)
  (define (iter set1 set2 rst)
    (cond ((null? set1) (append rst set1))
          ((null? set2) (append rst set2))
          (else (let ((x1 (car set1)) (x2 (car set2)))
                  (cond
                   ((= x1 x2)
                    (iter (cdr set1) (cdr set2) (append rst (list x1 x2))))
                   ((< x1 x2) 
                    (iter (cdr set1) set2 (append rst (list x1))))
                   ((< x2 x1)
                    (iter set1 (cdr set2) (append rst (list x2)))))))))
  (iter set1 set2 ()))

(union-set '(1 2 2 4) '(2 2 3 4))
;;tree
(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x () ()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(adjoin-set 2 (adjoin-set 1 ()))

;;2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons () elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                   (right-result (partial-tree (cdr non-left-elts)
                                               right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry left-tree right-tree)
                          remaining-elts))))))))

(list->tree '(1 3 5 9 7 11))

;; 2.65
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))

(define (union-set-tree tree1 tree2)
  (let ((list1 (tree->list tree1)))
    (define (adjoin-list-to-tree list tree)
      (cond ((null? list) tree)
            (else
             (adjoin-list-to-tree (cdr list)
                                  (adjoin-set (car list) tree)))))
    (list->tree (adjoin-list-to-tree list1 tree2))))

(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set
               (tree->list tree1)
               (tree->list tree2))))

(union-set-tree (list->tree '(1 3 5)) (list->tree '(2 4)))
                                        ;Value 613: ((1 () ()) (2 () ()) ((4 (3 () ()) (5 () ())) () ()))

(intersection-set-tree (list->tree '(1 3 5)) (list->tree '(1 3 5 9 7 11)))
                                        ;Value 612: (3 (1 () ()) (5 () ()))
;;2.66
(define (make-entry key value) (cons key value))
(define (key entry) (car entry))
(define (value entry) (cdr entry))
(define test-tree (list->tree (list
                               (make-entry 1 "aa")
                               (make-entry 2 "bb")
                               (make-entry 3 "cc")
                               (make-entry 4 "dd")
                               (make-entry 5 "ee")
                               )))

(define (loopup x tree)
  (cond ((null? tree) false)
        ((= x (key (entry tree)))
         (value (entry tree)))
        ((< x (key (entry tree))) 
         (loopup x (left-branch tree)))
        ((> x (key (entry tree))) 
         (loopup x (right-branch tree)))))

(loopup 1 test-tree)
;;
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        ()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit"))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      ()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(make-leaf-set (list '(A 4) '(B 2) '(C 1) '(D 1)))

;;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode  sample-message sample-tree)
;;2.68

(define (encode message tree)
  (if (null? message)
      ()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-in-tree? symbol tree)
  (cond ((leaf? tree) (eq? symbol (symbol-leaf tree)))
        (else (memq symbol (symbols tree)))))

(define (encode-symbol message tree)
  (define (encode-iter message subtree result)
    (cond ((null? message) result)
          ((leaf? subtree) (encode-iter (cdr message) tree result))
          (else (let ((left (left-branch subtree))
                      (right (right-branch subtree)))
                  (cond ((symbol-in-tree? (car message) left)
                         (encode-iter  message left (cons '0 result)))
                        ((symbol-in-tree? (car message) right)
                         (encode-iter  message right (cons '1 result)))
                        (else (error "sambol not in the tree"))
                        )))))
  (encode-iter message tree ()))

(define sample-message '(a d a b b c a))
(encode-symbol sample-message sample-tree)
(restart 1)
;; 2.69
(make-leaf-set (list '(A 4) '(B 2) '(C 1) '(D 1)))
(define (successive-merge pairs)
  (cond ((= 1 (length pairs)) pairs)
        (else
         (successive-merge
          (adjoin-set
           (make-code-tree (car pairs) (cadr pairs))
           (cddr pairs))))))

(successive-merge (make-leaf-set (list '(A 4) '(B 2) '(C 1) '(D 1))))
;; 2.70
(define sing-pairs (list '(a 2) '(na 16) '(boom 1) '(sha 3) '(get 2) '(yip 9) '(job 2) '(wah 1)))
(define sing-tree (car (successive-merge (make-leaf-set sing-pairs))))
(encode-symbol '(Get a job) sing-tree)
                                        ; (0 0 0 0 0 0 0 0 0 1 1 1)
(encode-symbol '(Sha na na na na na na na na) sing-tree)
                                        ; (0 1 1 1 1 0 0 1 1 1 1 1 1 1)
(encode-symbol '(Wah yip yip yip yip yip yip yip yip yip) sing-tree)
                                        ; (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1)
(encode-symbol '(Sha boom) sing-tree)
                                        ; (1 1 0 1 1 0 1 1 1)
;;2.71
(successive-merge (make-leaf-set (list '(a 1) '(b 2) '(c 4) '(d 8) '(e 16))))
;;2.4.2
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (real-part-rectangular z) (car z))
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unkown type -- REAL_PART" z))))
;;
;;---------------------------------------------------
(define local-table (list '*table*))
(define (assoc key records)
  (cond ((null? records)
         #f)
        ((equal? key (caar records))
         (car records))
        (else
         (assoc key (cdr records)))))
(define (get key-1 key-2)
  (let ((subtable (assoc key-1 (cdr local-table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))
(define (put key-1 key-2 value)
  (let ((subtable (assoc key-1 (cdr local-table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr local-table)))))
  'ok)
;;--------------------------------------------------
(define (tag x) (attach-tag 'rectangular x))
(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  ;;interface rest of the system
  (put 'real-part '(rectangular) real-part)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(rectangular) real-part))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No mathod for these type -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(install-rectangular-package)
(real-part (attach-tag 'polar  (cons 1 2)))

;; 2.73

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp)) (operands exp) var))))

(define (install-deriv-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (car exp) var)
              (deriv (cadr exp) var)))
  (define (deriv-product exp var)
    (make-sum
     (make-product (car exp)
                   (deriv (cadr exp) var))
     (make-product (deriv (car exp) var)
                   (cadr exp))))
  (define (exponentiation exp var)
    (make-product (cadr exp)
                  (make-exponentiation
                   (car exp)
                   (make-sum (cadr exp) -1))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** exponentiation))
(install-deriv-package)
;; test
(deriv '(** x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y) 'x)
(restart 1)
;; 2.74
(define (get-tb table key-1 key-2 )
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))
(define (put-tb table key-1 key-2 value)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (employee name)
  (define rst (list '*table*))
  (put-tb rst name 'address 'my-address)
  (put-tb rst name 'salary '999)
  rst)

(define (load-offices)
  (define office-table (list '*table))
  (put-tb office-table 'office-1 'name1 (employee 'name1))
  (put-tb office-table 'office-2 'name2 (employee 'name2))
  (put-tb office-table 'office-2 'name3 (employee 'name3))
  (put-tb office-table 'office-3 'name2 (employee 'name2))
  office-table)

(define offices-record (load-offices))

(define (get-record office name)
  (get-tb offices-record office name))

(define (get-salary office name)
  (define employee (get-tb offices-record office name))
  (if (not employee)
      (error "CANNOT FIND THE EMPLOYEE!")
      (get-tb employee name 'salary)))

(define (find-employee-record name)
  (define (iter table name result)
    (if (null? table)
        result
        (let ((subtable (car table)))
          (if (null? subtable)
              result
              (let ((record (assoc name (cddr (cadr subtable)))))
                (if record
                    (iter (cdr table) name (cons result record)
                    (iter (cdr table) name result)))))))
    (iter (cdr offices-record) name '()))

  (find-employee-record 'name3)
(restart 1)

(get-record 'office-2 'name2)
(get-salary 'office-1 'name2)

;; message pass
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y))))

(define (apply-generic op arg) (arg op))
;;2.75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part) (* x (sin y)))
          ((eq? op 'imag-part) (* x (cons y)))
          (else
           (error "Unknown op -- MAKE FROM MAG ANG" op))))
  dispatch)

((make-from-mag-ang 1 2) 'magnitude)
;;
(define (add x y) (apply-generic 'add x y))
;;2.77
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  'done)
(install-scheme-number-package)



