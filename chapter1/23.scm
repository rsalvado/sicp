; Exercise 1.23. The smallest-divisor procedure shown at the start of this section does lots of
; needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if
; it is divisible by any larger even numbers. This suggests that the values used for test-divisor should
; not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement this change, define a procedure next
; that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest-
; divisor procedure to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run the test for
; each of the 12 primes found in exercise 1.22. Since this modification halves the number of test steps, you
; should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio
; of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (next-divisor number)
    (if (even? number) (+ number 1) (+ number 2)))

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
; 100000007 *** 5.0000000000000044e-2
; 100000037 *** 2.9999999999999916e-2
; 100000039 *** 3.0000000000000027e-2
; 
; (search-for-primes-improved 1000000000 3)
; 1000000007 *** .13
; 1000000009 *** .07999999999999996
; 1000000021 *** .06000000000000005
; 
; (search-for-primes-improved 10000000000 3)
; 10000000019 *** .20999999999999996
; 10000000033 *** .21999999999999997
; 10000000061 *** .19999999999999996
; 
; (search-for-primes-improved 100000000000 3)
; 100000000003 *** .7300000000000002
; 100000000019 *** .6699999999999999
; 100000000057 *** .6699999999999999

; Improvement ratio
; 0.07 / 0.11 = 1.57
; 0.33 / 0.2 = 1.65
; 1.07 / 0.66 =  1.62

; Possible explanation: overhead of the next-divisor function call and the if statement
; inside it.