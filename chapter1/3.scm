; Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(define (sum-of-squares a b c)
  (cond ((and (> a b) (> b c)) (+ (square a) (square b)))
        ((and (> b a) (> c a)) (+ (square b) (square c)))
        (else (+ (square a) (square c)))))