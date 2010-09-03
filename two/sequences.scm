;; 2.2.1 Representing Sequences


(car (list 1 2 3 4))
(cdr (list 1 2 3 4))
(car (cdr (list 1 2 3 4)))
(cdr (cdr (list 1 2 3 4)))
(car (cdr (cdr (list 1 2 3 4))))

(define my-list (list 1 2 3 4))
(define squares (list 1 4 9 16))

;; Return the nth element in a list (0-based)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref my-list 2)

;; Return the length of a list
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length my-list)

;; Append l2 to l1
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))
(append my-list squares)


;; Return last-pair of a list
(define (last-pair l)
  (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))
(last-pair my-list)

(define x (list))
(define y (cons (cdr (list 1))
		(cdr (list 1))))
(null? y))
(null? x)

(cdr (list 1))
(cons (cdr (list 1)) (list 1))
(null? (cdr (list 1)))

;; Return the reversed list
(define (reverse l)
  (if (null? (cdr l))			;Reverse of list (1) -> (1)
      l
      (append (reverse (cdr l))
	      (list (car l)))))
(reverse squares)

;; Dotted-tail notation
(define (same-parity first . l)
  (define (return-same-parity l)
    (if (null? l)
	(list)
    (if (= (remainder (car l) 2) (remainder first 2))
	(cons (car l)
	      (return-same-parity (cdr l)))
	(return-same-parity (cdr l)))))
  (cons first (return-same-parity l)))

(same-parity 2 3 4 5 6 7 8)

;; Scale items in a list by given factor
(define (scale-list l factor)
  (if (null? l)
      l
      (cons (* factor (car l))
	    (scale-list (cdr l)
			factor))))
(scale-list my-list 10)

;; Mapping over lists
(cons (list)
      5)

;; For-each
(define (my-for-each fn l)
  (if (null? l)
      #t
      (let ()
	(fn (car l))
	(my-for-each fn (cdr l)))))
(my-for-each (lambda (x)
	       (newline)
	       (display (* x x))) my-list)

;; Accumulate
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
	  (accumulate op init (cdr seq)))))

(accumulate + 0 (list 1 2 3 4))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
(accumulate-n * 1 (list (list 1 2)
			(list 3 4)))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(transpose (list (list 1 2 3)
		 (list 4 5 6)
		 (list 7 8 9)))
(define mat1 (list (list 1 2)
		   (list 3 6)))

(define mat2 (list (list 6 -2)
		   (list -3 1)))



(define (matrix*matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
	   (map (lambda (y)
		  (dot-product y x))
		cols))
	 m)))

(define (dot-product v1 v2)
  (accumulate + 0 (map * v1 v2)))

(matrix*matrix mat1 mat2)

(define (fold-left op init seq)
  (define (iter init seq)
    (if (null? seq)
	init
	(iter (op (car seq)
		  init)
	      (cdr seq))))
  (iter init seq))

(fold-left - 0 (list 1 2 3 4))
(accumulate - 0 (list 1 2 3 4))
(accumulate list (list) (list 1 2 3))
(fold-left list (list) (list 1 2 3))

(append (list) (list 1 2) )

(define (append l x)
  (cond 
   ((null? l)
    (cond ((null? x) x)
	  ((not (pair? x))
	   (list x))
	  (else x)))
   (else (cons (car l)
	       (append (cdr l)
		       x)))))
(append (list 1 2 3) (list 1 2 3))
(append (list) 3)

(define (reverse-sequence seq)
  (accumulate (lambda (x y)
		(if (null? y)
		    (list x)
		    (append y x)))
	      (list)
	      seq))

(reverse-sequence (list 1 2 3 4))

(define (reverse seq)
  (fold-left (lambda (x y)
	       (cons x y))
	     (list)
	     seq))
(reverse (list 1 2 3 4))

(define (enumerate-interval a b)
  (if (> a b)
      (list)
      (cons a (enumerate-interval (+ 1 a)
			      b))))
(enumerate-interval 1 10)

(define (filter predicate seq)
  (if (null? seq)
      seq
      (if (predicate (car seq))
	  (cons (car seq)
		(filter predicate (cdr seq)))
	  (filter predicate (cdr seq)))))

(filter (lambda (x)
	  (= (remainder x 2)
	     1))
	(list))


(define n 10)

(list (list) (list 1 2))
(define pairs
  (accumulate append
	      (list) 
	      (map (lambda (x)
		     (map (lambda (y) (list x y))
			  (enumerate-interval 1 (- x 1))))
		   (enumerate-interval 1 10))))


(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

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

(is-prime? 12)

(filter (lambda (pair)
	  (let ((sum (+ (car pair)
			(cadr pair))))
	    (is-prime? sum)))
	pairs)

(cadddr (list 1 2 3 4 5))


