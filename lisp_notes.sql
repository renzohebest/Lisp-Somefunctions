; single-line comments begin with a semi-colon.

#| multiline comments begin with
    #|
   and end with 
   |#
   hey, look - they can be nested!
|#


;; ------------------------------------------------------------

;; literal values evaluate to themselves.

;; integers
5
-123
0

;; single- and double-precision floats
3.14159s0
1.23451234512345d0

;; rationals (fractions). Cool/weird!
2/3

;; complex numbers...
#c(3 4)

;; our stand-ins for true and false
T
NIL

;; string literals
"hello"


;; ------------------------------------------------------------

#| 
   many common operators exist. We apply them, and any functions,
   in prefix order, adjacent to their arguments, in parentheses.
   Parentheses always have a meaning, so don't just go adding more!
|#

(+ 2 3)

(- 10 4)

(* 6 7)

(/ 42 5)


;; modulus/remainder, absolute value
(mod 12 5)
(mod -1 5)

(rem 12 5)
(rem -1 5)

(abs -13)

;; max/min accept one or more arguments.
(max -10 -13 -5)
(min 6 8 4 5 12)


;; to chain multiple things together, we still have to use
;; prefix notation every time!

(+ (* 2 3) 4)


;; boolean operators
(and T T T)
(and T T T T NIL T T)

(or NIL NIL NIL T NIL)

(not T)

;; relational operators
(> 3 5)
(>= 2 3)
(< 2 3 4 5)  ;; whoah, it checked (and (< 2 3) (< 3 4) (< 4 5))
(<= 5 6)

;; for primitive types, we can use =, but there's a bigger
;; story to tell about equality checks. Later on.
(= 4 4)


;; String concatenation is a bit messy. must mention the type first.
(concatenate 'string "abc" "def" "ghi")
(concatenate 'list '(1 2 3) '(4 5))

;; for lists, append is preferable.
(append (list 1 2 3) (list 4 5))

;; ------------------------------------------------------------

#|

 defining functions:

(defun  <funcName> ( params go here )
  "optional documentation string"
  bodyExpression
)
|#

(defun inc (x)
  "adds one to its argument"
  (+ x 1)
)


(inc 4)
(inc 20)

#|

if-expressions:
 (if cond thenbranch elsebranch)

of course we can nest 'em anywhere expressions are accepted.
|#

(defun max3 (x y z)
  "chooses biggest of three"
  (if (and (>= x y) (>= x z)) x
    (if (>= y z) y z )))


;; recursion is definitely gonna be our game.
(defun fact (n)
  (if (<= n 1) 1
    (* n (fact (- n 1))))) ;; just call itself with new arguments.


#|

Look at how we relate the loop version to the tail-recursive version below.
python version:
def fact(n):
   if n<=1: return 1
   prod = 1
   for i in range(n, -1, -1):
      prod *= i
   return prod
|#

(defun fact-taily (acc n)
  (if (zerop n) acc
    (fact-taily (* n acc) (- n 1))))

#|
multiple if's? Use cond. Feed it (expr expr) 

(cond
       (boolExpr1 expr1)
       (boolExpr2 expr2)
       ...
)
|#

;; ------------------------------------------------------------

(defun max4 (a b c d)
     (cond ((and (>= a b) (>= a c) (>= a d)) a)
	   ((and (>= b c) (>= b d))          b)
	   ((>= c d)                         c)
	   (T d) ;; true condition behaves as our "else" branch.
	   ))



;; ------------------------------------------------------------

#|

let-expressions: let us bind variables to values.  NOTE: all exprs are
evaluated, *then* they all get assigned! (if you want them performed
sequentially to use each other, then you need the let* form).

(let (                   ;; or, use let*
      (var1 expr1)
      (var2 expr2)
      ...
     )
     body
)

|#

;; just a silly example...
(defun max5 (a b c d e)
  (let (
	(ab (max a b))
	(cd (max c d))
       )
       (max3 ab cd e)
) )

;; ------------------------------------------------------------

;; LISTS  (This is the LISt Processing language, after all!)


;; we can feed multiple values to list. 
(list 2 4 1 3 5)

;; we can quote something (') to avoid evaluating it, leaving it as a
;; list until we need it later. here, some undeclared variables are
;; sneaking into our list value. Don't evaluate them here!
'(a b c)


#|

cons: accepts the head element and the tail (rest of the list).
(cons expr expr)

NIL: used to mean an empty list.

car: deconstructs a cons and gives back the head element.
(car expr)

cdr: deconstructs a cons and gives back the tail list.
(cdr epr)

first/rest: identical to car/cdr.
(first expr)
(rest expr)

|#

(cons 1 (cons 2 (cons 3 NIL)))

(car '(10 15 20 25))
(cdr '(10 15 20 25))

(first (list 2 4 6))
(rest  (list 2 4 6))

;; ------------------------------------------------------------

;; useful predicate functions. Often (but not always), named as
;; a claim and the letter p (for "predicate").

(zerop 0)
(zerop 5)

(null (list 1 2 3))
(null NIL)
(null (list))

(consp NIL)          ;; is NIL a cons cell? no.
(consp (list 1 2 3)) ;; does this list begin with a cons? yep.

(evenp 4)   ;; even check
(oddp 5)    ;; odd check

;; ------------------------------------------------------------

;; structural recursion on lists: step through head item per recursive
;; call.

(defun my-length (xs)
  (if (null xs) 0
    (+ 1 (my-length (cdr xs)))))

(defun my-sum (xs)
  (if (null xs) 0
    (+ (car xs) (my-sum (cdr xs)))))

(defun evens (xs)
  (cond ((null xs)   NIL)
	((evenp (car xs))   (cons (car xs) (evens (cdr xs))))
	(T (evens (cdr xs)))
	))
	
;; ------------------------------------------------------------

;; tracing! turn on tracing of a function to see all its calls,
;; no matter who called it (you or another function call).

#|
(trace my-length)
(my-length (list 1 2 3 4 5))
(untrace my-length)
|#


;; trace all the relevant functions when debugging, in batches.

#|
(trace my-length my-sum)
(untrace my-length my-sum)
|#

;; all this tracing will muddy up the loading of this file... don't worry about it.

;; ------------------------------------------------------------

;; dribbling: record this portion of all terminal output.
(dribble "thisfile.txt")

;; do some stuff...
(my-length (list 1 2 3))
(my-sum  '(1 2 3 4 5))

;; turn off dribbling, save/close the file.
(dribble)

;; ------------------------------------------------------------

(defun filter (p xs)
  (if (null xs) NIL
    (if (funcall p (car xs)) (cons (car xs) (filter p (cdr xs)))
      (filter p (cdr xs)))))

(filter #'evenp (list 1 2 3 4 5 6))

;; ------------------------------------------------------------

#|

lambdas: we can build up a function in-place:

(lambda (params here) body)

|#

;; let's find all the numbers above 5 in the list 3->8.
(filter (lambda (x) (> x 5)) (list 3 4 5 6 7 8))


;; ------------------------------------------------------------

;; first-order functions: to pass a function as an argument,
;; we have to put #' in front of it.


;; mapcar: given a function and a list, apply the function to each
;; item in the list, and put these results into a new list that we get
;; back.
(mapcar #'inc (list 10 11 12))
(mapcar (lambda (x) (* x 4)) (list 10 11 12))


;; maplist: keep applying the given function to each successive tail
;; list. Kindof odd...
(maplist #'my-sum (list 1 2 3 4))

;; ------------------------------------------------------------


#|

Python version:

def min-max(xs):
   if len(xs)==0: return None
   min = xs[0]
   max = xs[0]
   for i in range(len(xs)):
      if (min>xs[i]):
          min = xs[i]
      if (max<xs[i]):
          max = xs[0]
   return (min, max)
|#

;; helper functions: if you wanted a loop with specific variables in
;; use, instead make a helper function with the extra parameters.

(defun choose-smaller (a b)
  (if (< a b) a b))

(defun choose-bigger (a b)
  (if (> a b) a b))


;; if we wanted to make one pass over the list and find the minimum
;; and maximum value in the list, we'd need to keep track of both
;; values while walking along the list. That means we need an extra
;; parameter for each.
(defun min-max-helper (minval maxval xs)
  ;; if the list is empty, give back our current best knowledge of
  ;; minimum and maximum values.
  (if (null xs) (list minval maxval)
    ;; when the list wasn't empty, let's make these bindings:
    (let* (
	   (x (car xs))                         ;; head of the list
	   (therest (cdr xs))                   ;; tail of the list
	   (minval (choose-smaller minval x))   ;; smaller of x and our current smallest-seen value
	   (maxval (choose-bigger  maxval x))   ;; larger  of x and our current largest-seen value
	   )
      ;; recursively call ourselves with the updated min/max values,
      ;; on the tail of the list. (we're done with the value at the
      ;; head of this current list node).
      (min-max-helper minval maxval therest))))

;; this was our entire goal. It relies upon the previous few
;; functions.
(defun min-max (xs)
  (if (null xs) NIL
    (min-max-helper (car xs) (car xs) xs)))

;; ------------------------------------------------------------


#|
what's the meaning of various equality checks?

 = : only for numbers. converts freely between them to compare.

 eq : checks for them being the same object (same symbol).

 eql : allows = or eq. (it's like eq, but relaxed for characters and
 numbers (nums must be same type; 3 != 3.0))

 equal: must be structurally similar (print out the same)
    - basically, if they are eql or make the same list
|#


;; ------------------------------------------------------------

;; "special global" values

;; declares *foo* to have the value 5.  we didn't have to include the
;; *'s in the name, but it's convention.
(defvar       *foo*  5)
(defparameter *bar* 10)

;; defvars can't be changed... so juse defparameter.
(defvar       *foo* 15)    ;; still 5!
(defparameter *bar* 20)    ;; changes to 20.

;; as usual, don't get carried away with globals.

;; ------------------------------------------------------------

;; sequencing: if you do printing (print expr), or create variables,
;; or otherwise want to sequence other actions, we can always glue
;; together a few with progn. It evaluates them in turn, and then
;; returns the value of the last one.

#|
(progn
  (print "hello ")
  (print "world!")
  )
|#


;; we might not need this much in our current classwork since we're
;; not trying to write particularly imperative code..

;; ------------------------------------------------------------

;; MACROS: we can define new _syntax_ in our language! Don't think of
;; other languages' use of the word "macro". We're just showing what
;; pattern of stuff will be fed to this function-like thing, and then
;; how to deconstruct it. Since all our programs are basically lists
;; (they're all parenthesized things), it's really trivial to create 

#|
;; taken from:
;; http://www.gigamonkeys.com/book/macros-standard-control-constructs.html

(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

...but these are already in the language as special operators, so we
can't define them on our own again.

|#

#|
(when (> 5 3)             ;; one conditional...
  (print "using the")     ;; as many more forms as we want.
  (print "when-macro.")   ;; like this extra one.
  )
|#

;; some other macros:
;; AND, OR, COND, DOLIST, DOTIMES, LOOP

;; ------------------------------------------------------------
