;;3.1
(define (make-accumulator ini-val)
  (lambda (amount)
    (begin
      (set! ini-val (+ amount ini-val))
      ini-val)))

(define A (make-accumulator 1))
(A 1) ; val->2
(A 2) ; val->4
;;3.2
(define (make-monitored proc)
  (define count 0)
  (define (how-many) count)
  (define (reset)
    (begin
      (set! count 0)
      count))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) (how-many))
          ((eq? m 'reset-count) (reset))
          (else
           (begin
             (set! count (+ 1 count))
             (proc m)))))
  dispatch)
;; Test
(define b (make-monitored sqrt))
(b 100)
(b 100)
(b 'how-many-calls?) ; val->2
(b 'reset-count)     ; val->1

;;3.3 & 3.4
(define (make-account balance password)
  (define error-pw-cnt 0)
  (define (call-the-cops)
    (display "Calling cops......."))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw password) 
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (begin
          (set! error-pw-cnt (+ 1 error-pw-cnt))
          (if (>  error-pw-cnt 7)
              (call-the-cops)
              (error "Invalid password" pw)))))
  dispatch)

(define acc (make-account 100 'pw))
((acc 'foo 'withdraw ) 50)
((acc 'pw 'withdraw ) 50)  ; 50

;;
(define rand
  (let ((x 1))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (random 10000) (random 10000)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(estimate-pi 2000)

;; ex3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-pi trials x1 y1 x2 y2)
  (define radio (min (/ (abs (- x2 x1)) 2) (/ (abs (- y2 y1)) 2)))
  (define area (* (monte-carlo trials radio x1 y1 x2 y2)
                  (* (abs (- x2 x1)) (abs (- y2 y1)))))
  (/ area (expt radio 2)))

(define (radio-test radio x1 y1 x2 y2)
  (define x (- (random-in-range x1 x2) (/ (- x2 x1) 2)))
  (define y (- (random-in-range y1 y2) (/ (- y2 y1) 2)))
  (< (+ (expt x 2) (expt y 2)) (expt radio 2)))

(define (monte-carlo trials radio x1 y1 x2 y2)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((radio-test radio x1 y1 x2 y2)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(estimate-pi 2000 0 0 4 4.0)



