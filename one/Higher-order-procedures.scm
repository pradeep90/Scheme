;; Chapter 1 Problems
;; Date : 11 Aug 2010
;; Mainly higher-order procedures

(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (-1 x)))))

(factorial 4)
;; Factorial (iterative version)
(define (factorial x)			
  (define (fact-iter fact i)
    (if (= 1 i)
	fact
	(fact-iter (* i fact) (- i 1))))
  (fact-iter 1 x))

(factorial 4)
;; Fast exponent (iterative)
;; For iterative processes, a*b^n is a constant
;; ie. some product is a constant is a powerful idea
(define (fast-exp b n)
  (define (fast-exp-iter a b n)
    (cond ((= n 0) a)
	  ((= (remainder n 2) 1) (fast-exp-iter (* a b) b (- n 1)))
	  ((fast-exp-iter a (* b b) (/ n 2)))))
  (fast-exp-iter 1 b n))

(fast-exp 2 5)
(< 2 1)

(define (summation fn a next b)		;General higher-order procedure
  (if (> a b)
      0
      (+ (fn a) (summation fn (next a) next b))))

(summation (lambda (x) x) 1 (lambda (x) (+ 1 x)) 10)
(/ 355 113)

;; Nice problem
(define (double fn)
  (lambda (x)
    (fn (fn x))))
(define (inc x)
  (+ 1 x))
(((double double) inc) 3)
(((double (lambda (x) (+ (x) 4))) inc) 3)
(inc (inc (inc (inc (inc (inc (inc (inc 3))))))))


(define (new-double fn)
  (lambda (x) (fn (fn (fn (fn x))))))

((new-double inc) 3)
(((double new-double) inc) 5)
(((new-double new-double) inc) 5)


(((double (double double)) inc) 4)
(double double) 
(((double (double double)) (lambda (x) (+ 1 x))) 5)

      
  
;; Turns out the 'double' question meant just do it twice
;; Not composition

(define (double fn)
  (lambda (x)
    (* (fn x) 2)))

((double inc) 2))

(define (compose fn1 fn2)
  (lambda (x)
    (fn1 (fn2 x))))
(define (square x)
  (* x x))
((compose square inc) 6)

(define (repeated fn n)			;Iterative process
  (define (repeat-compose final fn n)
    (if (= n 1)
      final
      (repeat-compose (compose fn final) fn (- n 1))))
  (repeat-compose fn fn n))

((repeated square 2) 5)
  



