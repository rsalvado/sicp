; Exercise 1.7. The good-enough? test used in computing square roots will not be very effective for
; finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost
; always performed with limited precision. This makes our test inadequate for very large numbers. Explain
; these statements, with examples showing how the test fails for small and large numbers. An alternative
; strategy for implementing good-enough? is to watch how guess changes from one iteration to the
; next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that
; uses this kind of end test. Does this work better for small and large numbers?

(define (sqrt x)
  (sqr-iter 1.0 x))

(define (sqr-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqr-iter (improve guess x)
                x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

; The defined tolerance 0.001 can be too large when dealing with small numbers
; (sqrt 0.0001) -> 0.0001 instead of 0.01
; When numbers are big enough machine precission is not sufficient to detect small differences between numbers and the algorithm never terminates.

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x)))
     (* guess 0.01)))

;TODO: Work a bit more on this one.