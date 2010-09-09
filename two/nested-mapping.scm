;; Nested Mapping
;; Date : 28 August 2010

;; Permutation Function - Mine - Version 1
(define (permutation s)
  ;; Return list of permutations of s

  (define (append-to-every-perm first-term)
    ;; Append to first-term every perm of s-{first-term}
    ;; Return list of permutations with first-term at the front
    ;; ie. (append-to-every-perm f), where s : (2 f 3)
    ;; ((f 2 3) (f 3 2))
    (let ((s-dash (filter (lambda (y)
			    (not (= first-term y)))
			  s)))
      (map (lambda (z)
      	     (append first-term z))
      	   (let ((perm (permutation s-dash)))
      	     perm))
      ))

  (cond ((null? s) (list s)) 		;List of one empty permutation
	((null? (cdr s)) (list s))
	;; Note : flatmap is the key here, map is not to be used here
	(else (flatmap append-to-every-perm s))))

(define perm (permutation (list 1 2 3 4 5)))



;; Permutation - SICP - STUDMAX!!
(define (permutation s)
  (if (null? s)
      (list s)
      (flatmap (lambda (x)
		 (map (lambda (perm-dash)
			(append x perm-dash))
		      (permutation (remove x s))))
	       s)))

(define (remove x s)
  (filter (lambda (t)
	    (not (= t x)))
	  s))


(for-each (lambda (x)
	    (display x)
	    (newline))
	  perm)
(filter is-prime? (list 4))


;; Unique Pairs
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (list j i))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
(unique-pairs 3)
  

(define (ordered-triplets n)
  (flatmap (lambda (i)
	     (map (lambda (pair)
		    (append pair i))
		  (unique-pairs (- i 1))))
	   (enumerate-interval 1 n)))
(ordered-triplets 5)


;; Queens Problem

(define (queens k)
  ;; Return list of valid positions seqs of queens
  ;; pos-seq i = position of queen in ith row of that arrangement
  (define (queens-rows m k)
    ;; Return list of arrangements for queens in first m rows of k cols
    (let ())
    (filter (lambda (arrangement)
	      (safe? k arrangement))
	    (let ((solution-m-1 (queens-rows (- m 1) k)))
	      (flatmap (lambda (mth-row-posn)
			 (map (lambda (m-1-arrangement)
				(append m-1-arrangement mth-row-posn))
			      solution-m-1))
		       (enumerate-interval 1 k))))
  (queens-rows k k)))

(define (queens k)
  (define (queens-rows m k)
    (if (= m 0)
	(list (list))
	(filter (lambda (arrangement)
		  (safe? m arrangement))
		(flatmap (lambda (prev-soln)
			   (map (lambda (new-row)
				  (append prev-soln new-row))
				(enumerate-interval 1 k)))
			 (queens-rows (- m 1) k)))))
  (queens-rows k k))

(queens 8)
(length (queens 7))
(and (list 1 2 5 3)
     (list 5 6)
     (list 1 2 3))
(and (list)
     (= 2 2))
(if #f
    (list 1 2)
    (list 2 3))

(define (safe? m arrangement)
  (let ((right-diag-sums (map + arrangement (enumerate-interval 1 m)))
	(left-diag-diffs (map - arrangement (enumerate-interval 1 m)))
	(new-posn (list-ref arrangement (- m 1))))
    (let ((new-posn-diag-sum (list-ref right-diag-sums (- m 1)))
	  (new-posn-diag-diff (list-ref left-diag-diffs (- m 1))))
      (and (= (length (filter (lambda (x)
				(= x new-posn))
			      arrangement)) 1)
	   (= (length (filter (lambda (x)
				(= x new-posn-diag-sum))
			      right-diag-sums)) 1)
	   (= (length (filter (lambda (x)
				(= x new-posn-diag-diff))
			      left-diag-diffs)) 1)))))
      

    
    (and (unique-elems? right-diag-sums)
	 (unique-elems? left-diag-diffs)
	 (unique-elems? arrangement))))
(safe? 8 (list 6 3 1 7 5 8 2 4))
(safe? 1 (list 1))

(define (unique-elems? arr)
  (define (present-in? arr x)
    (cond ((null? arr) #f)
	  ((pair? x) #f)
	  ((= (car arr) x) #t)
	  (else (present-in? (cdr arr) x))))
  (cond ((null? arr) #t)
	((present-in? (cdr arr) (car arr)) #f)
	(else (unique-elems? (cdr arr)))))

(unique-elems? (list ))
	
(safe? 3 (list 1 2 3))