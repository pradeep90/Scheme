;; 2.1.4 Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (upper-bound y)
	    (lower-bound y))
	 0)
      (error "Cannot divide by interval of width 0")
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (sub-interval int1 int2)
  (let ((d1 (- (upper-bound int1) (lower-bound int2)))
	(d2 (- (lower-bound int1) (upper-bound int2))))
    (make-interval d2 d1)))

(define (print-interval int)
  (newline)
  (display "(")
  (display (lower-bound int))
  (display ",")
  (display (upper-bound int))
  (display ")")
  (newline))

(print-interval (make-interval 5 10))
(let ((int1 (make-interval 5 10))
      (int2 (make-interval -2 7)))
  (print-interval (add-interval int1 int2))
  (print-interval (sub-interval int1 int2))
  (print-interval (mul-interval int1 int2))
  (print-interval (div-interval int1 int2)))

;; The same "interval" compound data object is used
;; Just that this is another way of constructing an interval
;; and of viewing the contents in a different form

(define (make-center-percent center-point percent-tolerance)
  (make-interval (- center-point (/ (* percent-tolerance center-point)
			      100))
		 (+ center-point (/ (* percent-tolerance center-point)
			      100))))

(define (center interval)
  (/ (+ (upper-bound interval) (lower-bound interval)) 2))

(define (percent interval)
  (* 100
     (/ (- (upper-bound interval) (center interval))
	(center interval))))
(define (print-center-percent interval)
  (newline)
  (display "(")
  (display (center interval))
  (display ",")
  (display (percent interval))
  (display ")")
  (newline))

(print-center-percent (make-center-percent 100 8))

;; 
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(let ((r1 (make-center-percent 100 1))
      (r2 (make-center-percent 100 2)))
  (define test (lambda (fn)
		 (print-center-percent (fn r1 r2))))
  (test par1)
  (test par2)
  (test div-interval)
  (print-center-percent (div-interval r1 r1))
  (print-center-percent (div-interval r2 r2)))

