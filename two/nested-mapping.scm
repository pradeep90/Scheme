;; Nested Mapping
;; Date : 28 August 2010

(define (permutation s)
  (map 
   (lambda (x)
     (let ((s-dash (filter (lambda (y)
			     (not (= x y)))
			   s)))
       (map (lambda (z)
	      (append (list x)
		      z))
	    (permutation s-dash))))
   s))

(permutation (list 1 2 3))
(map (lambda (x)
       (display x)) (list "HI" "HI" "HI"))
(filter is-prime? (list))
