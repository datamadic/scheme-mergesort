; xavier oneil 
; Merge Sort in Scheme
; 2015.3.2


; return the list up to the index passed in, () if index is 
; out of bounds    
;
; @param {list} lst
; @param {number} index
; @return {list} 
(define until
    (lambda (lst index)
      (if (and 
	   (> index 0)
	   (> (length lst) index))
	  (cons (car lst) (until (cdr lst)(- index 1)))
	  '())))


; return the list after the index passed in 
;
; @param {list} lst
; @param {number} index
; @return {list} 
(define after 
  (lambda (lst index)
    (if (null? lst) lst
    (if (eq? 0 index )
	lst
	(after (cdr lst) (- index 1))))))


; return a list containing the passed in list split in half
; the car is the first half, the cdr is the second 
;
; @param {list} lst
; @return {list} 
(define split 
  (lambda (lst)
    (if (eq? (length lst) 1)
	lst
    (let* ((middle (floor (/ (length lst) 2)))
	   (first (until lst middle))
	   (second (after lst middle)))
      (cons first second)))))


; return a list sorted least to greatest from the combination
; of the lists passed in 
;
; @param {list} l1
; @param {list} l2
; @return {list} 
(define merge 
  (lambda (l1 l2)
    (cond 
     ((null? l1) l2)
     ((null? l2) l1)
     ((<= (car l1) (car l2))
	  (cons (car l1) (merge (cdr l1) l2)))
     ((cons (car l2) (merge (cdr l2) l1))))))


; return a list sorted least to greatest 
;
; @param {list} lst
; @return {list} 
(define mergesort 
  (lambda (lst)
    (if (<= (length lst) 1)
	lst
	(let ((left (car (split lst)))
	      (right (cdr (split lst))))
	  (merge (mergesort left) (mergesort right))))))


; Some examples
;(until '(1 2 3 4) 2)
;(after '(1 2 3 4 ) 2)
;(split '(1 2 3))
;(split '(1))
;(split '(1 2 4 3 4 8 2 90 87 3 99 ))
;(cdr (split '(1 2 4 3 4 8 2 90 87 3 99 1)))
;(merge '(1 3 2 4 6) '(0 0 9 9 9))
;(merge '(1 3 4 5 7 9 11 14 63) '(2 4 6 8 100))
;(merge '(2 14) '(2))
;(merge (car (split '(1 5 5 1))) (cdr (split '(1 5 5 1))))
;(mergesort '(2 0 1))
;(mergesort '(1 5 5 30 1 0 0 0 1  293 74 83 72 83 72 91 9))




     
     