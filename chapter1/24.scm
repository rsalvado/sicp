; Exercise 1.24. Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the
; Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has
;   (log n) growth, how would you expect the time to test primes near 1,000,000 to compare with the time
; needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                     m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

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
             (found (fast-prime? n 1))
             (elapsed-time (- (runtime) start))
             (left (or (and found (- times 1)) times)))
        (and found (report-prime n elapsed-time))
        (search-for-primes-improved (next n) left))))
