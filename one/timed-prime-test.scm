;; Timed Prime Tester
;; Ex. 1.22
;; Date - 12 August 2010
;; SUCCESS!!

;Prime tester
(define (is-prime? n)			
  (define (test-prime-factor k)
    (define (is-factor? k)
      (= (remainder n k) 0))
    (if (> (* k k) n)
	#t
	(if (is-factor? k)
	    #f
	    (test-prime-factor (+ k 1)))))
  (test-prime-factor 2))

(is-prime? 15)



;; Get total microseconds elapsed b/w a & b
(define (subtract-pair a b)		
  (let ((a1 (car a))
	(a2 (cdr a))
	(b1 (car b))
	(b2 (cdr b)))
    ;; (display a1)
    ;; (newline)
    ;; (display a2)
    ;; (newline)
    ;; (display b1)
    ;; (newline)
    ;; (display b2)
    ;; (newline)
    (- (+ a2 (* 1000000 a1))
       (+ b2 (* 1000000 b1)))))

(define now (gettimeofday))
;; Time taken for prime-testing a number	
(define (timed-prime-test n)
  ;; (newline)
  ;; (display "n ")
  ;; (display n)
  (start-prime-test n (gettimeofday)))

(define (start-prime-test n start-time)
  (if (is-prime? n)
      (report-prime n (gettimeofday) start-time)
      #f))
(define (report-prime n end-time start-time);elapsed-time)
  (newline)
  (display "n ")
  (display n)
  (display " time ")
  ;; (display start-time)
  ;; (newline)
  ;; (display end-time)
  ;; (newline)
  (display (subtract-pair end-time start-time))
  )

;  (display elapsed-time))

(if (timed-prime-test 10)
    (display "OK")
    (display "TOO BAD!"))


;; Procedure to be used for comparing time taken in prime-testing
(define (search-for-primes start end)
  (define (search-iter start end ctr)
    (cond ((> start end) (display "Bye"))
	  ((= ctr 3) (newline) (display "Successful!"))
	  (else (if (timed-prime-test start)
		    (search-iter (+ 2 start) end (+ 1 ctr))
		    (search-iter (+ 2 start) end ctr)))))
  (search-iter (if (= (remainder start 2) 0)
		   (+ 1 start)
		   start)
	       end
	       0))
(search-for-primes 1000 2000)
(search-for-primes 10000 20000)
(search-for-primes 100000 200000)
(search-for-primes 1000000 2000000)
