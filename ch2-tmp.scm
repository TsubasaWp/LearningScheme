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
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))b

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
(print-list (permutations (list 1 2 3 4)))

;; 2.40
(enumerate-interval 1 10)
(define (unique-pairs low high)
  (flatmap (lambda (x) 
         (map (lambda (y) (list x y))
              (enumerate-interval (+ x 1) high)))
           (enumerate-interval low high)))
(unique-pairs 1 3);;=> ((1 2) (1 3) (2 3))
(define (prime-sum-pair n)
  (filter prime-sum? (unique-pairs 1 n)))
(prime-sum-pair 3)

;; 2.41
(define (unique-triples low high)
  (flatmap (lambda (x)
             (map (lambda (i) (append x i)) (unique-pairs (+ x 1) high)))
           (enumerate-interval low high)))
(unique-triples 1 4)
(define (prime-sum-triple? list)
  (prime? (+ (car list) (cadr list) (cadr (cdr list)))))
(define (prime-sum-triples n)
  (filter prime-sum-triple? (unique-triples 1 n)))
(prime-sum-triples 4) ;;=> ((1 2 4) (2 3 4))

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

;; round-list
(define (equal-pair p1 p2)
  (cond ((and (= (car p1) (car p2)) (= (cdr p1) (cdr p2))) #t)
        ((and (= (car p1) (cdr p2)) (= (cdr p1) (car p2))) #t)
        (else #f)))

(define (empty-rounds team-size)
  (if (= team-size 0)
      (list)
      (append (list (list)) (empty-rounds (- team-size 1)))))
(empty-rounds 3)

;; round问题
(define (round-list team-size)
  (define (round-col k)
    (if (= 0 k)
        (empty-rounds team-size)
        (flatmap
         (lambda (rest)
           (map (lambda (next)
                  (adjoin next k rest))
                (enumerate-interval 1 team-size)))
         (round-col (- k 1)))
        ))
  (round-col team-size))
(round-list 4)

(define (round-col k)
  (if (= 0 k)
      (empty-rounds 4)
      (map
       (lambda (rest)
         (map (lambda (next)
                (append rest (list next))
                )
              (enumerate-interval 1 4)))
       (round-col (- k 1)))
      ))
(round-col 2)

(define (adjoin next k rest)
;;  (if (not (= k next))
      (list (cons k next) rest))
;;  (list rest))
  (map (lambda (next)
         (adjoin next 3 (list (cons 1 2))))
       (enumerate-interval 1 3))

;; 赛程
(define (pairmap proc seq)
  (cond ((null? seq) (list))
        ((null? (cdr seq)) (list))
        (else (proc (cons (car seq) (cadr seq))
                    (pairmap proc (cdr (cdr seq)))))))
(pairmap (lambda (p next) (append (list p) next)) (list 1 2 3 4))

(define (permutation-pair s)
  (map
   (lambda (seq)
     (pairmap (lambda (p next)
                (append (list p)  next))
              seq))
   (permutations s)))

(print-list (permutation-pair (list 1 2 3 4)))

(define (equal-pair? p1 p2)
  (cond ((and (= (car p1) (car p2)) (= (cdr p1) (cdr p2))) #t)
        ((and (= (car p1) (cdr p2)) (= (cdr p1) (car p2))) #t)
        (else #f)))

(define (equal-pair-list? s1 s2)
  (null?
   (filter
    (lambda (p1)
      (null?
       (accumulate
        (lambda (p2 next)
          (if (equal-pair? p1 p2)
              (append (list p1) next)
              next))
        (list)
        s2)))
    s1)))

(equal-pair-list? (list (cons 1 2) (cons 4 3)) (list (cons 2 1) (cons 4 3)))
(restart 1)

(define (unique-pair-list sequence)
  (if (null? sequence)
      (list (list (cons 0 0)))
      (let ((rest (unique-pair-list (cdr sequence))))
        (append rest
                (accumulate
                 (lambda (x y)
                   (if (equal-pair-list? (car sequence) x)
                       (list)
                       y))
                 (list (car sequence))
                 rest)))))

(print-list
 (unique-pair-list (permutation-pair (list 1 2 3 4 5 6))))

(restart 1)

(define (team-division n base)
  (cond ((= n 2) (cons base (+ base 1)))
        (else (append
               (list (team-division (/ n 2) base))
               (list (team-division (/ n 2) (+ base (/  n 2))))))))

(define (team2 a b)
  (cons (a b)))

(define (team-div team-lst)
  (cond (()))
  (paral-team team-lst)
  (cross-team team-lst))

(define (paral-team team-lst)
  (team)



