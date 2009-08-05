; Exercise 1.25. Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After
; all, she says, since we already know how to compute exponentials, we could have simply written
; (define (expmod base exp m)
;     (remainder (fast-expt base exp) m))
; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.

; Original expmod

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                     m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                     m))))

; Using fast-expt

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))

; We could have used new-expmod but it is considerably slower than expmod. fast-expt has to deal with huge intermediate results maybe that slows the process.