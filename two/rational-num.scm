;; Compound data objects - Rational Numbers

(define (make-rat n d)
  (let ((gcd-n-d (gcd (abs n) (abs d))))
    (if (< d 0)
	    (cons (/ (- n) gcd-n-d) (/ (- d) gcd-n-d))
	    (cons (/ n gcd-n-d) (/ d gcd-n-d)))))
    
  (cons n d ))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (gcd a b)
  (if (< a b)
      (gcd b a)
      (cond ((= b 0) a)
	    ((= b 1) 1)
	    ((gcd b (remainder a b))))))
(gcd (- 40) 10)

(numer (make-rat 1 2))
(denom (make-rat 1 2))
(print-rat (make-rat  10 -40))

;; Exercise 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-coord p))
  (display ",")
  (display (y-coord p))
  (display ")")
  (newline))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (make-point x y)
  (cons x y))

(define (x-coord p)
  (car p))

(define (y-coord p)
  (cdr p))

(print-point (make-point 1 2))

(define (avg x y)
  (/ (+ x y) 2))
(define (mid-point-segment l)
  (print-point (make-point (avg (x-coord (start-segment l)) (x-coord (end-segment l )))
			   (avg (y-coord (start-segment l)) (y-coord (end-segment l))))))

(mid-point-segment (make-segment 
				 (make-point 1 1)
				 (make-point 3 5)))

;; Exercise 2.3

;; Rectangles
;; Constructor
(define (make-rect topleft bottomright)
  (cons topleft bottomright))

;; Selectors
(define (top-left rect)
  (car rect))

(define (bottom-right rect)
  (cdr rect))

(define (perimeter rect)
  (* 2 (+ (- (x-coord (bottom-right rect)) (x-coord (top-left rect)))
	  (- (y-coord (top-left rect)) (y-coord (bottom-right rect))))))


