;; Representation of pairs using higher-order procedures

(define (cons x y)
  (lambda (fn) (fn x y)))

(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))

(define (sum z)
  (z +))

(define (product z)
  (z *))

(product (cons 2 3))


;; Representation of pairs using only numbers and arithmetic!!! 
;; pair (a, b) = 2^a * 3^b

(define (fast-exp x n)
  (define (fast-exp-iter x n value)
    (if (= n 0)
	value
	(if (= (remainder n 2) 1)
	    (fast-exp-iter x (- n 1) (* value x))
	    (fast-exp-iter (* x x) (/ n 2) value))))
  (fast-exp-iter x n 1))

(fast-exp 5 2)


(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (car z)
  (power-of 2 z))

(define (cdr z)
  (power-of 3 z))

(define (power-of x num)
  ;;(x^total *num = NUM, for every iteration)
  (define (power-of-iter total x num)
    (if (= (remainder num x) 0)
	(power-of-iter (+ 1 total) x (/ num x))
	total))
  (power-of-iter 0 x num))

(cdr (cons 2 3))

;; Numbers in terms of procedures
;; n is a procedure that applies f to x n times
(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define (f x)
  (+ 1 x))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define (add-special a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(((add-special one one) f) 4)
(((add-1 one) f) 4)
((one f) 4)
