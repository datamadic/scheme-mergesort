(define pi (* 4 (atan 1.0)))


(define torad 
  (lambda (deg)
    (* deg (/ pi 180))))

(define afterx
  (lambda (rate time)
    (string-append (number->string (* rate time)) " miles")))

(define fact
  (lambda (x)
    (if (= x 0) 1
	( * x  (fact (- x 1))))))

(define ladd 
  (lambda (x)
    (let ((y 1))
      (+ y x))))



    
(define until
    (lambda (lst intex)
      (if
       (and 
	(> intex 0)
	(> (length lst) intex))
       (cons (car lst) (until (cdr lst)(- intex 1)))
       '())))

(define after 
  (lambda (lst index)
    (if (null? lst) lst
    (if (eq? 0 index )
	lst
	(after (cdr lst) (- index 1))))))

(define split 
  (lambda (lst)
    (if (eq? (length lst) 1)
	lst
    (let* ((middle (floor (/ (length lst) 2)))
	   (first (until lst middle))
	   (second (after lst middle)))
      (cons first second)))))

(split '(1 2 3))
(split '(1))

(define merge 
  (lambda (l1 l2)
    (cond 
     ((null? l1) l2)
     ((null? l2) l1)
     ((<= (car l1) (car l2))
	  (cons (car l1) (merge (cdr l1) l2)))
     ((cons (car l2) (merge (cdr l2) l1))))))

(merge '(1 3 2 4 6) '(0 0 9 9 9))
(merge '(1 3 4 5 7 9 11 14 63) '(2 4 6 8 100))
(merge '(2 14) '(2))

(split '(1))

(define mergesort 
  (lambda (lst)
    (if (or (eq? (length lst) 1)
	    (null? lst))
	lst
	(let ((left (car (split lst)))
	      (right (cdr (split lst))))
	  (merge (mergesort left) (mergesort right))))))

(mergesort '(2 0 1))


(mergesort '(1 5 5 30 1 0 0 0 1  293 74 83 72 83 72 91 9))
(split '(1 2 4 3 4 8 2 90 87 3 99 ))
(car (split '(1 2 4 3 4 8 2 90 87 3 99 1)))
(cdr (split '(1 2 4 3 4 8 2 90 87 3 99 1)))
(define lst '(1 5 5 1))
(car (split '(1 5 5 1)))
(cdr (split '(1 5 5 1)))
(merge (car (split '(1 5 5 1))) (cdr (split '(1 5 5 1))))

;(define merge 
;  (lambda (l1 l2)
;    (let ((firstL1 (car l1))
;	  (firstL2 (car l2)))
;      (cond 
;       ((> firstL1 firstL2) (cons firstL1 firstL2))
;       (

;    (cond 
;     ((> (car l1) (car l2)) (cons 


;    (cond 
;     ((null? l1) (

;(define mergsort 
;  (lambda (lst)
;    (cond
;     ((eq? (length lst) 1) (car lst))
     
     