; Exercise 1.22. Most Lisp implementations include a primitive called runtime that returns an integer
; that specifies the amount of time the system has been running (measured, for example, in microseconds).
; The following timed-prime-test procedure, when called with an integer n, prints n and checks to
; see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in
; performing the test.
; (define (timed-prime-test n)
;    (newline)
;    (display n)
;    (start-prime-test n (runtime)))
; (define (start-prime-test n start-time)
;    (if (prime? n)
;           (report-prime (- (runtime) start-time))))
; (define (report-prime elapsed-time)
;    (display " *** ")
;    (display elapsed-time))
; Using this procedure, write a procedure search-for-primes that checks the primality of consecutive
; odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000;
; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime.
; Since the testing algorithm has order of growth of ( n), you should expect that testing for primes
; around 10,000 should take about        10 times as long as testing for primes around 1000. Do your timing
; data bear this out? How well do the data for 100,000 and 1,000,000 support the n prediction? Is your
; result compatible with the notion that programs on your machine run in time proportional to the number of
; steps required for the computation?

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))
(define (report-prime prime elapsed-time)
  (display prime)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes min max)
  (let ((min (if (even? min) (+ min 1)))) 
    (and (< min max)
         (timed-prime-test min)
         (search-for-primes (+ min 1) max))))

(define (search-for-primes-improved n times)
  (define (next number)
    (if (even? number) (+ number 1) (+ number 2)))
  (if (> times 0)
      (let* ((start (runtime))
             (found (prime? n))
             (elapsed-time (- (runtime) start))
             (left (or (and found (- times 1)) times)))
        (and found (report-prime n elapsed-time))
        (search-for-primes-improved (next n) left))))

; (search-for-primes-improved 100000000 3)
; 100000007 *** .05999999999999961
; 100000037 *** .03999999999999915
; 100000039 *** .02999999999999936
; 
; (search-for-primes-improved 1000000000 3)
; 1000000007 *** .1399999999999988
; 1000000009 *** .11000000000000121
; 1000000021 *** .09999999999999964
; 
; (search-for-primes-improved 10000000000 3)
; 10000000019 *** .33000000000000007
; 10000000033 *** .33000000000000007
; 10000000061 *** .33999999999999986
; 
; (search-for-primes-improved 100000000000 3)
; 100000000003 *** 1.1500000000000004
; 100000000019 *** 1.0700000000000003
; 100000000057 *** 1.08

; (sqrt 10) = 3.16
; (* 0.0399 3.16) = 0.126
; (* 0.126 3.16) = 0.39
; (* 0.39 3.16) = 1.23