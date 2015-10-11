#lang racket
(require "auto.rkt")

(provide (all-defined-out))

(define (set-immutable automaton posn new-value)
  (append
   (take automaton posn)
   (list new-value)
   (drop automaton (add1 posn))))

(define (mutate an-auto)
  (let ([flatten-one (flatten-automaton an-auto)]
        [r (random 41)]
        [c (random 3)]
	[s (random 10)])
    (if (member r (list 1 5 9 13 17 21 25 29 33 37))
        (make-automaton (set-immutable flatten-one r c))
	(make-automaton (set-immutable flatten-one r s))
        )))

(define (mutate-populations countdown population)
  (let* ([l (length population)]
         [r (random l)]
         [mutated (mutate (list-ref population r))]
         [new-population (set-immutable population r mutated)])
    (if (zero? countdown)
        population
        (mutate-populations (sub1 countdown) new-population))))
