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
    (generate-auto)))

(define A (create-population 100))

(define (round2 n)
  (/ (round (* 100 n))
     100))

(define rankings '())
(define population-mean '())
(define demographic '())
;; evolve the population over cycles
;; N=100
(define (evolve population cycles speed mutation rounds-per-match delta)
  (let* ([N (length population)]
         [round-results (match-population population rounds-per-match delta)]
         [average-payoff (exact->inexact (/ (apply + (flatten round-results))
                                            N))]
         [max-pay (apply max (flatten round-results))]
         [accum-fitness (accumulate (payoff-percentages (flatten round-results)))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [before-mutation
          (shuffle (append survivors successors))]
         [new-population
          (mutate-populations mutation before-mutation)]
         )
    (set! population-mean
          (append population-mean (list average-payoff)))
    (and (< average-payoff (* 3/4 max-pay))
         (out-rank cycles population delta))
    (if (zero? cycles)
        (begin
          (plot-mean population-mean delta)
          (out-mean population-mean delta)
          population)
        (evolve new-population (sub1 cycles) speed mutation rounds-per-match delta))))

(define B (evolve A 100 10 1 30 .2))
