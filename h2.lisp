;;;; hw2_rtejada 
;;; Renzo Tejada

;; Self Notes
;; Use od lamda
(setq number (1+ 2))
((lambda (number) (* 7 number)) 3)
;;
;;;
;;; HW #2

(proclaim '(optimize (debug 1)))
(defun fib (n &optional (x 0) (y 1) (total 1))
  (if (<= n 0)  x ;; Pre: n is 0 or negative return 0
  (fib (- n 1) y total (+ y total))));; else evaluate n-1

;;(concatenate 'list '(1 2 3) '(4 5))
;;(reversed *list*)

(defun reversed (list)
  (loop
     with result = nil 
     for element in list ;For loop lisp style 
     if (listp element) ; Returns true if object is of type list; otherwise, returns false.
        do (push (reversed element) result)
     else do (push element result)  ; once list is empty get rid of old list ptr
     finally (return result)))

(defun is_prime (n)
  (cond ((< n 2) nil) ;;From hw specs n can be any number that is less than 1 
    ((= 2 n) t) ;; 2 and 3 are by default prime and true is returned
    ((= 3 n) t)
      ((evenp n) nil)
      (t 
         (loop for i from 3 to (isqrt n) by 2
             never (zerop (mod n i)))))
)


(defun nub-helper (r_list xs) ; r_list holds new list, xs holds list being passed
    (cond
      ;(nub-helper  (cons (car xs) r_list) (cdr xs) )  
      ;( if t       (nub-helper  (cons (car xs) r_list) (cdr xs) ) ) 
       (
       		(not(member   (car xs)  r_list )) ; pre_con: head of xs can't be in r_list
       			(nub-helper  (cons (car xs) r_list) (cdr xs) ) ; action; push new element to head of the list
       )
       (; 							at this point premis is that (car xs) is a member already
	 		(member   (car xs)  r_list ) 
					;(nub-helper   r_list (cdr xs) )
					(if (cdr xs)
						(nub-helper r_list (cdr xs) )
						(reversed r_list)
					)
       	)   
    )
)

(defun nub (xs)
   (nub-helper nil xs)

)
 (nub '(1 2 3 4 5 6))
 (nub (list 1 1 1 2 2 3 4 4 4 4 4))
; 

(defun zip_with (fn xs ys)
;; (loop for i in '(1 2 3) do (print i))
	(zip_with-helper nil fn xs ys )
	; (loop for i in xs
	; 	for j in ys
	;     ;do (and(funcall  'cons  i j)(print i)(print j))
	;     do (cons (funcall  'cons  i j) r_list) 
	; )
)

(defun zip_with-helper (r_list fn xs ys)
   (loop for i in xs
		for j in ys
	    ;do (and(funcall  'cons  i j)(print i)(print j))
	    do (
	    	and(push (funcall fn i j) r_list)(print  r_list)(print  i)
	    )
	    ;when ( (null (car r_list)))
	)
   ; (r_list)

)



 (defun prince-of-clarity (w)
   "Take a cons of two lists and make a list of conses.
    Think of this function as being like a zipper."
   (do ((y (car w) (cdr y))
        (z (cdr w) (cdr z))
        (x '() (cons (cons (car y) (car z)) x)))
       ((null y) x)
     (when (null z)
       (cerror "Will self-pair extraneous items"
              "Mismatch - gleep!  ~S" y)
       (setq z y)))) 




(zip_with #'+ (list 1 2 3) (list 10 20 30))




 
;;

; (defvar lista '(1 1 1 2 2 3 4 4 4 4 4))
; ( if(not(member 5 lista))  (print "success"))
;(nub '(1 2 3 4 5 6))


; ; (defun nub-helper (xs r_list)
; ;   (if (not(null xs))
; ;         (cons (car xs) r_list)
; ;         (nub-helper ((cdr xs) r_list))
; ;   )
; ;   (t(r_list))
; ; )


; (defun nub (xs)
;   ; (nub-helper (xs nil))
; )
;   ; (cond  
;  ;  ; 1st case: r_list is empty
;  ;        ( 
;  ;          (null r_list)       
;   ;         (format t "~%Head of xs pushed to r_list ~%")
;  ;            ( cons (car xs) r_list)
;   ;     (nub (cdr xs) r_list)    
;  ;        )
;   ;     ;;2nd Case: if element in the head of xs is is not in list
;   ;     (
;   ;        ; (member 1 '(1 2 3)) ;;
;   ;           (format t "Not a member then add~%")
;   ;           ;(push (car xs) r_list)
;   ;           ;(nub (cdr xs) r_list)
;  ;        )
;   ;      ;;pr
;   ;     ;(
;   ;     ;   (not (null? x))
;   ;     ;     (nub (cdr xs) r_list)
;   ;     ; );; cae element is in result list but there is elements left in cx
;   ;     ; ( (t)(r_list))
;   ;)
; )

; ; (nub '(1 2 3 4 5 6))

; (defvar xs '(1 2 3 4))
; (defvar r_list '(1 2))

; (if(member 1 r_list)(format t "~%Head of xs pushed to r_listsssssaaa ~%"));;
; (format t "~%Head of xs pushed trfsdafasfdsa ~%");;
















; ; (defun max3 (x y z)
; ;   "chooses biggest of three"
; ;   (if (and (>= x y) (>= x z)) 
; ;       x
; ;     (if (>= y z) y
; ;        z )
; ;    )
; ;  )


; ; (cond 
; ;       (
; ;        (>= *age* 18) ; If T do this
; ;           (setf *college-ready* 'yes)
; ;           (format t "Ready for College ~%")
; ;       )

; ;       (
; ;        (< *age* 18) ; Else If T do this
; ;           (setf *college-ready* 'no)
; ;           (format t "Not Ready for College ~%")
; ;       )
      
; ;       (
; ;         t 
; ;           (format t "Don't Know ~%")
; ;       )
; ; ) 


; ;(defun list-reverse (L)
; ;   "Create a new list containing the elements of L in reversed order."
; ;   (list-reverse-aux L nil))

; ; (defun list-reverse-aux (L A)
; ;   "Append list A to the reversal of list L."
; ;   (if (null L)
; ;       A
; ;     (list-reverse-aux (rest L) (cons (first L) A)
; ;     )
; ;   ) 
; ;  )






