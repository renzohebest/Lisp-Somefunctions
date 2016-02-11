;;;; hw2_rtejada 
;;; Renzo Tejada
(defun fib (n &optional (x 0) (y 1) (total 1))
  (if (<= n 0)	x ;; Pre: n is 0 or negative return 0
	(fib (- n 1) y total (+ y total))));; else evaluate n-1

;;(concatenate 'list '(1 2 3) '(4 5))
;;(reversed *list*)

(defun reversed (list)
  (loop
     with result = nil 
     for element in list ;For loop lisp style 
     if (listp element) ; Returns true if object is of type list; otherwise, returns false.
   		  do (push (reversed element) result)
     else do (push element result)	; once list is empty get rid of old list ptr
     finally (return result)))

(defun is_prime (n)
	(cond ((< n 2) nil) ;;From hw specs n can be any number that is less than 1 
		((= 2 n) t) ;; 2 and 3 are by default prime and true is returned
		((= 3 n) t)
     	((evenp n) nil)
     	(t 
       	 (loop for i from 3 to (isqrt n) by 2
             never (zerop (mod n i))))))



(defun list-reverse (L)
  "Create a new list containing the elements of L in reversed order."
  (list-reverse-aux L nil))

(defun list-reverse-aux (L A)
  "Append list A to the reversal of list L."
  (if (null L)
      A
    (list-reverse-aux (rest L) (cons (first L) A)
    )
  ) 
 )


(defparameter *age* 19) ; Create variable age

(defvar *college-ready* nil)
 
;;

(defvar lista '(1 1 1 2 2 3 4 4 4 4 4))
( if(not(member 5 lista))  (print "success"))
;; (nub '(1 2 3 4 5 6))



(defun nub (xs &optional (r_list '()))
	
	(cond 
  ; 1st case: r_list is empty
        ( 
          (null r_list)       
			       (format t "~%Head of xs pushed to r_list ~%")
             (push (car xs) r_list)
			       (nub (cdr xs) r_list)    
        )
		  ;;2nd Case: if element in the head of xs is is not in list
		  ; (
		  ; 	if(not(member (car xs) r_list));;
		  ; 		(progn
		  ; 			(format t "Not a member then add~%")
		  ; 			(push (car xs) r_list)
		  ; 			(nub (cdr xs) r_list)
		  ; 		)
		  ;  );;pr
		  ; (
		  ; 	(not (null? x))
		  ; 		(nub (cdr xs) r_list)
		  ; );; cae element is in result list but there is elements left in cx
		  ; ( (t)(r_list))
	)
)

(nub '(1 2 3 4 5 6))

(defun max3 (x y z)
  "chooses biggest of three"
  (if (and (>= x y) (>= x z)) 
  		x
    (if (>= y z) y
    	 z )
   )
 )


(cond 
      (
       (>= *age* 18) ; If T do this
		      (setf *college-ready* 'yes)
		      (format t "Ready for College ~%")
      )

  	  (
       (< *age* 18) ; Else If T do this
  		    (setf *college-ready* 'no)
		      (format t "Not Ready for College ~%")
      )
  	  
      (
        t 
          (format t "Don't Know ~%")
      )
) 







