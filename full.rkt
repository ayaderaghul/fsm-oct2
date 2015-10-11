 #lang racket
(provide (all-defined-out))

(require "auto.rkt"
         "fit.rkt"
         "match.rkt"
         "mutation.rkt"
         "scan.rkt"
	"test.rkt"
         "tv.rkt"
	"out.rkt")


;; generate population
(define (create-population N)
  (for/list
      ([n N])
    (create-automaton)))

(define A (create-population 100))

(define (round2 n)
  (/ (round (* 100 n))
     100))


;; evolve the population over cycles
;; N=100

(define (evolve population cycles speed mutation rounds-per-match
                delta file-list)
  (define-values (result popu)
    (for/fold ([result '()]
               [population population])
              ([i cycles])
      [define N (length population)]
      [define round-results (match-population population rounds-per-match delta)]
      [define total (apply + (flatten round-results))]
      [define average-payoff (exact->inexact (/ total N))]
      [define max-payoff (apply max (flatten round-results))]
      [define accum-fitness (accumulated-payoff-percentages (flatten round-results))]
      [define survivors (drop population speed)]
      [define successors (randomise-over-fitness accum-fitness population speed)]
      [define before-mutation
        (shuffle (append survivors successors))]
      [define new-population
        (mutate-populations mutation before-mutation)]
      (out-rank i new-population 10 (second file-list))
      (values (cons average-payoff result)
              new-population)))
  (out-mean (reverse result) (first file-list))
  (plot-mean (reverse result) (third file-list)))


;; run mass

(define (run-one s r d)
  (define B (create-population 100))
  (define name-list (n->srd s r d))
  (time
   (evolve B 100 s 1 r d name-list)))


(define (run-many list-of-speeds list-of-rounds list-of-deltas)
  (for* ([i (in-list list-of-speeds)]
         [j (in-list list-of-rounds)]
         [k (in-list list-of-deltas)])
    (run-one i j k)))
