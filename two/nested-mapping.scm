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