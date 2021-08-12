;; Returns #t if x is a prime number, otherwise #f.
(define (prime? x)
  (define (iter factor1 factor2)
    (cond ((and (>= factor1 x) (>= factor1 x)) #t)
          ((= (* factor1 factor2) x) #f)
          ((>= factor2 x) (iter (+ factor1 1) 2))
          (else (iter factor1 (+ factor2 1)))))
  (iter 2 2))

;; Returns a stream containing all prime numbers starting from x.
(define (prime-numbers-from x)
  (if (prime? x)
      (cons x (delay (prime-numbers-from (+ x 1))))
      (prime-numbers-from (+ x 1))))

;; A stream containing all prime numbers.
(define prime-numbers (prime-numbers-from 1))

;; Returns the prime number following x.
(define (next-prime x) (car (prime-numbers-from (+ x 1))))

;; Unfolds the first n elements of the stream stream into a list.
(define (stream->list stream n)
  (if (or (<= n 0) (null? stream))
      '()
      (cons (car stream)
            (stream->list (force (cdr stream)) (- n 1)))))

(define (print-addition addend1 addend2 sum)
  (display addend1) (display " + ") (display addend2) (display " = ") (display sum) (newline))

;; Call this procedure to finally find out whether Goldbach's conjecture is correct. If the procedure
;; returns #f, the conjecture is false, if the procedure never halts, the conjecture is correct.
(define (is-goldbachs-conjecture-correct?)
  (define (iter even-num prime1 prime2)
    (cond ((and (>= prime1 even-num) (>= prime2 even-num)) #f)
          ((= (+ prime1 prime2) even-num) (begin (print-addition prime1 prime2 even-num)
                                                 (iter (+ even-num 2) 1 1)))
          ((>= prime2 even-num) (iter even-num (next-prime prime1) 2))
          (else (iter even-num prime1 (next-prime prime2)))))
  (iter 4 (car prime-numbers) (car prime-numbers)))

