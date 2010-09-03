;; 2.2.2 Trees

(define x (cons (list 1 2) (list 3 4)))

(null? 2)

(define (count-leaves l)
  (cond ((null? l) 0)
	((not (pair? l)) 1)
	(else (+ (count-leaves (car l))
		 (count-leaves (cdr l))))))
(count-leaves (list x x x))

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7)
			  9))))))

(car (car (list (list 7))))

(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))


(append x y)
(append x y)
(cons x y)
(list x y)

;; Write about - My imaginary representations of
;; lists and the various operations on lists
;; (eg. cons, car, cdr, append, reverse) on paper


(define (deep-reverse l)
  ;; (display l)
  ;; (newline)
  ;; (cond	((null? (cdr l)) l)
  ;; The above line caused the procedure to reverse a tree only a level deep
  (if (null? (cdr l))
      (if (not (pair? (car l)))
	  l
	  (list (deep-reverse (car l))))
      (append (deep-reverse (cdr l))
	      (if (not (pair? (car l)))
		  (list (car l))
		  (list (deep-reverse (car l)))))))

;; Deep Reverse - Revisited
(define (deep-reverse l)
  (append ()))
(deep-reverse (list 1 2  3 (list 1 2 3 4) 4))
(deep-reverse (list 1 2 3 4))
(cdr 2)
(cons (cons 1 (list))(list)) 
(append (append (list 1 2) (list)) (list))

;; Fringe of a list
;; Return all leaves in left-to-right order
(define (fringe l)
  (cond ((not (pair? l)) (error "Not a Tree!!"))
	(else (append (if (not (pair? (car l)))
			  (list (car l))
			  (fringe (car l)))
		      (if (null? (cdr l))
			  (list)
			  (fringe (cdr l)))))))
(define x (list (list 1 2)
		(list 3 4)))
(fringe (list x x))

;; Binary mobiles
(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

;; Branch
(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

(define (is-simple-branch? branch)
  (not (pair? (branch-structure branch))))


;; Total weight of a binary mobile
(define (total-weight mobile)
  (+ (if (is-simple-branch? (left-branch mobile))
	 (branch-structure (left-branch mobile))
	 (total-weight (branch-structure (left-branch mobile))))
     (if (is-simple-branch? (right-branch mobile))
	 (branch-structure (right-branch mobile))
	 (total-weight (branch-structure (right-branch mobile))))))

(define m (list (list 10 (list (list 1 9)
			       (list 3 3)))
		(list 12 10)))
(define n (make-mobile (make-branch 10 12)
		       (make-branch 10
				    (make-mobile (make-branch 2 4)
						 (make-branch 1 8)))))
(total-weight n)

(define (torque branch)
  (* (branch-length branch)
     (if (is-simple-branch? branch)
	 (branch-structure branch)
	 (total-weight (branch-structure branch)))))
(torque (car (cdr m)))

;; check if a binary mobile is balanced
(define (is-balanced? mobile)
  (if (not (= (torque (left-branch mobile))
	      (torque (right-branch mobile))))
      #f
      (and (if (is-simple-branch? (left-branch mobile))
	       #t
	       (is-balanced? (branch-structure (left-branch mobile))))
	   (if (is-simple-branch? (right-branch mobile))
	       #t
	       (is-balanced? (branch-structure (right-branch mobile)))))))
(is-balanced? n)

;; Scale Tree
(define (scale-tree tree factor)
  (cons (if (not (pair? (car tree)))
	    (* factor (car tree))
	    (scale-tree (car tree) factor))
	(if (null? (cdr tree))
	    (list)
	    (scale-tree (cdr tree) factor))))
(scale-tree-map m 2)

;; Scale Tree using Map
(define (scale-tree-map tree factor)
  (map (lambda (x)
	 (if (pair? x)
	     (scale-tree-map x factor)
	     (* factor x)))
       tree))

;; Square tree
(define (square-tree tree)
  (cons (if (not (pair? (car tree)))
	    (* (car tree)
	       (car tree))
	    (square-tree (car tree)))
	(if (null? (cdr tree))
	    (list)
	    (square-tree (cdr tree)))))
(deep-reverse (square-tree m))

;; Square tree using map
(define (square-tree-map tree)
  (map (lambda (x)
	 (if (not (pair? x))
	     (* x x)
	     (square-tree-map x)))
       tree))
(square-tree-map m)

;; Higher-order procedure - Tree-map
(define (tree-map fn tree)
  (map (lambda (x)
	 (if (not (pair? x))
	     (fn x)
	     (tree-map fn x)))
       tree))

(tree-map (lambda (x)
	    (* x x))
	  m)

;; Enumerate leaves of a Tree
(define (enumerate-leaves tree)
  (if (null? tree)
      (list)
      (append (if (not (pair? (car tree)))
		  (list (car tree))
		  (enumerate-leaves (car tree)))
	      (if (null? (cdr tree))
		  (list)
		  (enumerate-leaves (cdr tree))))))
(enumerate-leaves m)
