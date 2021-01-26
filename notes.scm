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

;; Some languages (Scheme, Haskell):
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


;;; exercise 1.10
;; Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; ...
; 2 ^ 10

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 2) = 2^2, (A 1 10) = 2 ^ 10 ==> (A 1 n) = 2^n
; 2 ^ (2 ^ 4)
; 2 ^ 16 = 65536

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 1 2))
; (2 ^ (2 ^ (2 ^ 2))
; 65536

;; (define (f n) (A 0 n)) => 2 * n
;; (define (g n) (A 1 n)) => 2 ^ n
;; (define (h n) (A 2 n)) => 2 ^^ n (tetration)
;; (define (k n) (* 5 n n)) => 5 * n ^ 2

(define (tetr a n)
  (define (tetr-iter val i)
    (if (= i n)
        val
        (tetr-iter (expt a val)
                   (+ i 1))))
  (tetr-iter 1 0))


;;; 1.2.2
;; Tree recursion

;; fibonacci numbers
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; The process 'branches' like a tree.
; This is a very inefficent procedure, because it does 
; so much redundant computation. The number of times (fib 1)
; is computed in (fib n) is equal to (fib (+ n 1)). The number
; of steps grows exponentially with the input, and the space 
; required grows linearly with the input.

;; iteratively:
;; a <- a + b
;; b <- a

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; This is linear iteration, it's much more efficent.
;; Tree recursion isn't always useless, but it's usually 
;; less efficent. One use case would be operating on
;; hierarchically structured data.

;; Although the tree recursive fib procedure is much less
;; efficent, it is more straightforward, because it is a 
;; direct translation of the definition of the Fibanocci 
;; sequence, the iterative procedure required more thinking.


;;; example: counting change
;; Write a procedure to compute the number of ways to change
;; any given amount of money.

(define (count-change amount)
  (define (coin-value coin-number)
    (cond ((= coin-number 1) 1)
          ((= coin-number 2) 5)
          ((= coin-number 3) 10)
          ((= coin-number 4) 25)
          ((= coin-number 5) 50)))

  (define (cc amount n-coins)
    (cond ((= amount 0) 1)
          ((or (= n-coins 0)
               (< amount 0)) 0)
          (else (+ (cc amount (- n-coins 1))
                   (cc (- amount (coin-value n-coins))
                       n-coins)))))
  (cc amount 5))

; That's REAL ugly...
; With a list of coins:
(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (null? coins)
             (< amount 0)) 0)
        (else (+ (cc amount
                     (cdr coins))
                 (cc (- amount (car coins))
                     coins)))))

;; These are tree recursive, and inefficent, but it isn't
;; obvious how to desing a better procedure for the problem.


;;; exercise 1.11

;; f(n) = | n				if n < 3
;;        | f(n-1) + 2f(n-2) + 3f(n-3)	if n >= 3

;; recursive process
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

; this is VERY inefficent

;; iterative process
(define (f-iterative n)
  (define (f-iter a b c count)
    (cond ((= count n) a)
          (else (f-iter b
                        c
                        (+ (* 3 a) (* 2 b) c)
                        (+ count 1)))))
  (f-iter 0 1 2 0))


;;; exercise 1.12
;; Pascal's triangle

; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1
; 1 5 10 10 5 1

; f(row, col) = | 1 if row == col or col == 0
;               | f(row-1, col-1) + f(row-1, col)

(define (pascal row col)
  (if (or (= row col) (= col 0))
      1
      (+ (pascal (- row 1) col)
         (pascal (- row 1) (- col 1)))))

; VERY inefficent

(define (pascal-iterative row col)
  (define (pascal-iter row-index nums)
    (if (= row-index 0)
        (list-ref nums col)
        (pascal-iter (- row-index 1)
                     (map + (cons 0 nums)
                          (append nums '(0))))))
  (pascal-iter row '(1)))

; much better


;;; exercise 1.13
;; Prove that fib(n) is the closest integer to f^n/sqrt(5),
;; where f = (1 + sqrt(5))/2.

;; Hint: let g = (1 − sqrt(5))/2, then
;; fib(n) = (f^n - g^n)/sqrt(5)

; fib(n) = fib(n-1) + fib(n-2)
; fib(n) = (f^(n-1) - g^(n-1))/sqrt(5) + (f^(n-2) - g^(n-2))/sqrt(5)
; ...


;;; exercise 1.14
;; Orders of growth of space and number of steps for count-change

; cc(n, 1) = O(n)
; cc(n, 2) = cc(n, 1) + cc(n-5, 2) = O(n^2)
; ...
; cc(n, k) = O(n^k)

; count-change => O(n^5) (steps)
;              => O(n)   (space)
