#lang sicp
(#%require "../simply.rkt")
(#%require racket/trace)

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond
      [(> (best-total dealer-hand-so-far) 21) 1]
      [(< (best-total dealer-hand-so-far) 17)
       (play-dealer customer-hand (se dealer-hand-so-far (first rest-of-deck)) (bf rest-of-deck))]
      [(< (best-total customer-hand) (best-total dealer-hand-so-far)) -1]
      [(= (best-total customer-hand) (best-total dealer-hand-so-far)) 0]
      [else 1]))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond
      [(> (best-total customer-hand-so-far) 21) -1]
      [(strategy customer-hand-so-far dealer-up-card)
       (play-customer (se customer-hand-so-far (first rest-of-deck))
                      dealer-up-card
                      (bf rest-of-deck))]
      [else
       (play-dealer customer-hand-so-far
                    (se dealer-up-card (first rest-of-deck))
                    (bf rest-of-deck))]))

  (let ([deck (make-deck)])
    (play-customer (se (first deck) (first (bf deck))) (first (bf (bf deck))) (bf (bf (bf deck))))))

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)))
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)))

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1))))
    (if (= size 0)
        deck
        (move-card deck '() (random size))))
  (shuffle (make-ordered-deck) 52))

(define (base-value cardword)
  (let ([val (butlast cardword)])
    (cond
      [(equal? 'A val) 1]
      [(member? val '(J Q K)) 10]
      [else val])))

(define (base-total hand)
  (accumulate + (every base-value hand)))

(define (has-ace? hand)
  (member? 'a (every bl hand)))

(define (best-total hand)
  (let ([base (base-total hand)])
    (if (and (has-ace? hand) (<= (+ 10 base) 21))
        (+ 10 base)
        base)))

(define (stop-at-17 hand dealer-up-card)
  (<= (best-total hand) 16))

(define (dealer-sensitive hand dealer-up-card)
  (let ([dv (base-value dealer-up-card)]
        [pv (best-total hand)])
    (or (and (member? dv '(1 7 8 9 10)) (< pv 17)) (and (member? dv '(2 3 4 5 6)) (< pv 12)))))

(define (play-n strategy n)
  (if (< n 1)
      0
      (+ (twenty-one strategy) (play-n strategy (dec n)))))

(define (stop-at n)
  (lambda (hand dealer-up-card) (<= (best-total hand) (dec n))))

(define (has-suit? hand suit)
  (member? suit (every last hand)))

(define (valentine hand dealer-up-card)
  (let ([strategy (if (has-suit? hand 'H)
                      (stop-at 19)
                      (stop-at 17))])
    (strategy hand dealer-up-card)))

(define (suit-strategy suit default-strat special-strat)
  (lambda (hand dealer-up-card)
    ((if (has-suit? hand suit) special-strat default-strat) hand dealer-up-card)))

(define (majority strat1 strat2 strat3)
  (lambda (hand dealer-up-card)
    (let ([r1 (strat1 hand dealer-up-card)]
          [r2 (strat2 hand dealer-up-card)]
          [r3 (strat3 hand dealer-up-card)])
      (or (and r1 r2) (and r1 r3) (and r2 r3)))))

(define (reckless strategy)
  (lambda (hand dealer-up-card) (strategy (butlast hand) dealer-up-card)))
