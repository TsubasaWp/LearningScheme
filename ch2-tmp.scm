(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f (n f) x))))

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda(x) x) x))))
(lambda (f) (lambda (x) (f x)))
; so, we can define the one
(define one (lambda (f) (lambda (x) (f x))))
(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
; so, we can define the two
(define two (lambda (f) (lambda (x) (f (f x)))))
;规律:one的定义中应用了一次f, two的定义中应用了2次
(define (add n)
  (lambda (f) (lambda (x) x)))
;; 2.3
(define (append list1 list2)
  (cond ((null? list1)
         list2)
        ((null? list2) ;; 对 list2=nil 的情况进行处理
         list1)
        ((and (not (pair? list1)) (not (pair? list2)))
         (list list1 list2)) ;; 两个都是简单数据时返回 list,而不是序对
        ((not (pair? list1))
         (cons list1 list2)) ;; list1 是简单数据时直接 cons,否则后面会对它 car/cdr
        (else
         (cons (car list1) (append (cdr list1) list2)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

;;2.3.2
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
(permutations (list 1 2 3))

