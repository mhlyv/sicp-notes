;;; exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;;; exercise 1.3
(define (sum-of-square-of-largest a b c)
  (cond ((< a b c) (+ (* b b) (* c c)))
	((< b a c) (+ (* a a) (* c c)))
	((< c a b) (+ (* a a) (* b b)))))


;;; exercise 1.4
(define (a-plus-abs-b a b)
  ; make sure b-s sign i + by choosing the procedure
  ((if (> b 0) + -) a b))


;;; exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; normal-order eval:
; (test 0 (p)) =>
; (if (= 0 0) 0 (p)) =>
; 0
; It applies the arguments to the expanded procedure

; applicative-order eval:
; (test 0 (p)) =>
; (test 0 (p)) =>
; ....
; It evaulates the aguments first


;;; example 1.1.7
;; Newton's sqrt

(define (sqr x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (newton-sqrt x)
  (sqrt-iter 1.0 x))


;;; exercise 1.6
;; if is a special form

; because procedure definitions are evaluated in
; applicative-order, both clauses are evaluated

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

; these work because number literals evaluate to themselves

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

; this will run forever, because it looks for a better guess
; even if the current guess is good enough

(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))


;;; exercise 1.7
;; good-enough?

(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) 0.001))

; the problem is the accepted error margin:
; (sqr (newton-sqrt 100)) => 100.00000000279795
; is pretty close, but in the case of
; (sqr (newton-sqrt 0.001)) => 0.0017011851721075596
; the result is almost double of the original value

;; An alterna-tive strategy for implementing good-enough? 
;; is to watch how guess changes from one iteration to the 
;; next and to stop when the change is a very small fraction 
;; of the guess.

(define (better-good-enough? guess x)
  (< (abs (- (sqr guess) x))
     (/ guess 10000)))

(define (better-sqrt-iter guess x)
  (if (better-good-enough? guess x)
      guess
      (better-sqrt-iter (improve guess x) x)))

(define (better-newton-sqrt x)
  (better-sqrt-iter 1.0 x))


;;; exercise 1.8
;; Newton's cube

(define (cube x)
  (* x x x))

(define (cube-root-better-guess guess x)
  (/ (+ (/ x (sqr guess))
	(* 2 guess))
     3))

; check with the same improved method

(define (good-enough-cube-root? guess x)
  (< (abs (- (cube guess) x))
     (/ guess 10000.0)))

(define (cube-root-iter guess x)
  (if (good-enough-cube-root? guess x)
      guess
      (cube-root-iter (cube-root-better-guess guess x) x)))

(define (newton-cube-root x)
  (cube-root-iter 1.0 x))


;; Localizing procedures to avoid scope pollution
;; and name conflicts.

(define (compact-newton-sqrt x)
  (define (sqr x)
    (* x x))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess x)
    (< (abs (- (sqr guess) x))
       (/ guess 10000.0)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x) x)))

  (sqrt-iter 1.0 x))

;; This can be further improved by using lexical scoping, 
;; because x is a free variable in the internal definitions.

(define (compact-newton-sqrt x)
  ; this doesn't change, because we need other squares too
  (define (sqr x)
    (* x x))

  ; this doesn't change either
  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess)
    (< (abs (- (sqr guess) x))
       (/ guess 10000.0)))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))

  (sqrt-iter 1.0))


;;; Linear Recursion and Iteration
;; factorial

;; n! = n * (n - 1)!

; direct translation to a procedure:
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; (factorial 6)
; (* 6 (factorial 5))
; (* 6 (* 5 (factorial 4)))
; (* 6 (* 5 (* 4 (factorial 3))))
; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
; (* 6 (* 5 (* 4 (* 3 2))))
; (* 6 (* 5 (* 4 6)))
; (* 6 (* 5 24))
; (* 6 120)
; 720

; this is a recursive process
; it queues up a chain of operations (deferred operations)
; the amount of information to keep track of grows linearly with n
; this kind of process is called linear recursion

;; different perspective:
;; product <- counter * product
;; counter <- counter + 1

(define (factorial n)
  (define (fact-iter product counter)
    (if (> counter n)
	product
	(fact-iter (* counter product)
		   (+ counter 1))))
  (fact-iter 1 1))

; (factorial 6)
; (fact-iter 1 1 6)
; (fact-iter 1 2 6)
; (fact-iter 2 3 6)
; (fact-iter 6 4 6)
; (fact-iter 24 5 6)
; (fact-iter 120 6 6)
; (fact-iter 720 7 6)
; 720

; This is an iterative process, it does not grow or shrink.
; All we need to keep track of, for any n, are the current
; values of the variables product, counter, and max-count.
; In general, an iterative process is one whose state can be
; summarized by a fixed number of state variables, with a fixed
; rule describing how the state variables should be updated as
; the process moves from state to state.

; In computing n!, the number of steps required grows linearly
; with n. Such a process is called a linear iterative process.

;; Note: recursive process != recursive procedure.
; A recursive procedure means a recursive syntax, but a recursive
; syntax doesn't necessarily mean a recursive process.

;; Common languages (Ada, Pascal, C):
; recursive procedure => recursive process
; Here iterative processes are achieved with special looping
; constructs (for, while).

;; Some languages (Lisp, Scheme, Haskell):
; recursive procedure =/=> recursive process (not always)
; An iterative process can be defined with a tail recursive
; procedure.


;;; exercise 1.9
;; recursive vs iterative processes

(define (dec x)
  (- x 1))

(define (inc x)
  (+ x 1))

(define (add a b)
  (if (= a 0)
      b
      (inc (add (dec a)
		b))))

; (add 4 5)
; (inc (add 3 5))
; (inc (inc (add 2 5)))
; (inc (inc (inc (add 1 5))))
; (inc (inc (inc (inc (add 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

; this is a recursive process

(define (add a b)
  (if (= a 0)
      b
      (add (dec a)
	   (inc b))))

; (add 4 5)
; (add 3 6)
; (add 2 7)
; (add 1 8)
; (add 0 9)
; 9

; this is an iterative process

; better version for numbers with different signed-ness

(define (add a b)
  (define a>0? (> a 0))
  (if (= a 0)
      b
      (add ((if a>0? dec inc) a)
	   ((if a>0? inc dec) b))))
