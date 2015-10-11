#lang racket
(require "match.rkt")

(provide (all-defined-out))

;; from the matching result, calculate the fitness
;; FITNESS
;; from matching result, calculate the fitness

(define (accumulated-payoff-percentages payoff-list)
  (define payoff-sum (apply + payoff-list))
  (define-values (accumulated _)
    (for/fold ([accumulated (list 0)]
               [init 0])
              ([y (in-list payoff-list)])
      [define next-init (+ init (/ y payoff-sum))]
      (values (cons next-init accumulated) next-init)))
  (reverse accumulated))

(define (randomise-over-fitness accumulated-payoff-percentage population speed)
  (for/list ([ n speed])
    [define r (random)]
    (for/and ([p (in-list population)]
              [a (in-list accumulated-payoff-percentage)]
              #:break (< r a))
      p)))
