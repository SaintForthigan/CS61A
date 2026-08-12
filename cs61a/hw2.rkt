#lang sicp
(#%require "../simply.rkt")
(#%require racket/trace)

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

(define (square x)
  (* x x))

(define (sum-sq-3 a b c)
  (let* ([il (list a b c)]
         [sql (map square il)]
         [sqsum (apply + sql)])
    (- sqsum (square (apply min il)))))

(define (sum-sq a b c)
  (let ([sqsum (+ (square a) (square b) (square c))]) (- sqsum (square (min a b c)))))

(define (dupls-removed ise)
  (cond
    [(empty? ise) '()]
    [(member? (first ise) (bf ise)) (dupls-removed (bf ise))]
    [else (se (first ise) (dupls-removed (bf ise)))]))

(define (make-adder num)
  (lambda (x) (+ x num)))

(define (substitute ise owd nwd)
  (cond
    [(empty? ise) '()]
    [(equal? (first ise) owd) (se nwd (substitute (bf ise) owd nwd))]
    [else (se (first ise) (substitute (bf ise) owd nwd))]))

(substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)

(define (t f)
  (lambda (x) (f (f (f x)))))

(define (s x)
  (+ 1 x))

;; Ex 1.31(a)

(define (identity x)
  x)

(define (factorial n)
  (product identity 1 inc n))

(define (pi-4 n)
  (define (term i)
    (cond
      [(even? i) (/ (+ 2 i) (+ 1 i))]
      [(odd? i) (/ (+ 1 i) (+ 2 i))]))
  (product term 1 inc n))

(define (pi n)
  (* 4.0 (pi-4 n)))

;; Ex 1.32(a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (filtered-accumulate predicate combiner null-value term a next b)
  (cond
    [(> a b) null-value]
    [(predicate a)
     (combiner (term a) (filtered-accumulate predicate combiner null-value term (next a) next b))]
    [else (filtered-accumulate predicate combiner null-value term (next a) next b)]))

(define (prime? n)
  #f ;; Usually true
  )

(define (sum-sq-primes n)
  (filtered-accumulate prime? + 0 square 2 inc n))

(define (rel-prod n)
  (define (pred i)
    (= 1 (gcd i n)))
  (filtered-accumulate pred * 1 identity inc (dec n)))

;; Ex 1.40
(define (average a b)
  (/ 2 (+ a b)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)
(define tolerance dx)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (= x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

;; Ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

;; Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)

;; Ex 1.43
(define (repeated f n)
  (if (<= n 1)
      f
      (compose f (repeated f (dec n)))))

;; Ex 1.46
(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    ((let iterate ([current-guess initial-guess])
       (if (good-enough? current-guess)
           current-guess
           (iterate (improve current-guess)))))))

;; HW 2

(define (every f il)
  (if (empty? il)
      '()
      (se (f (first il)) (every f (butfirst il)))))
(every square '(1 2 3))
(every (lambda (c) (word c c)) 'purple)
(keep even? '(781 2))
