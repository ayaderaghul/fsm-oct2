#lang racket
(require "auto.rkt")
(provide (all-defined-out))

(define (match-claims claims)
  (if (<= (apply + claims) 2)
      (map convert-payoff claims)
      (list 0 0)))

(define (convert-payoff x)
  (cond [(= x 0) 2]
        [(= x 1) 5]
        [(= x 2) 8]))

(define (match-pair auto1 auto2 rounds-per-match)
    (define round-results
      (for/fold ([round-result '()])
                ([i rounds-per-match])
        [define current-strat1 (automaton-current-strat auto1)]
        [define current-strat2 (automaton-current-strat auto2)]
        [define next-state1 (jump-to-state current-strat2 auto1)]
        [define next-state2 (jump-to-state current-strat1 auto2)]
        [define result (match-claims (list current-strat1 current-strat2))]
        (set! auto1 (update auto1 next-state1))
        (set! auto2 (update auto2 next-state2))
        (cons result round-result)))
    (reverse round-results))

;; in each match, take mean of round results for each automaton
;; returns a pair of means
(define (take-sums round-results)
  (map (lambda (f) (apply +  (map f round-results)))
       (list first second)))


(define (take-delta* f round-results delta)
  (let ([first-auto (map f round-results)])
    (for/list ([i (length first-auto)])
      (* (expt delta i) (list-ref first-auto i)))))

(define (take-delta round-results delta)
  (map (lambda (x) (apply + (take-delta* x round-results delta)))
       (list first second)))


(define (match-population population rounds-per-match delta)
  (define population-result
    (for/fold ([population-result '()])
              ([i (/ (length population) 2)])
      [define round-result
        (match-pair
         (list-ref population (* i 2))
         (list-ref population (add1 (* i 2)))
         rounds-per-match)]
      (cons
       (take-delta round-result delta)
       population-result)))
  (flatten (reverse population-result)))
