; #lang racket
;(provide A
;	create-population
;	evolve)

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
(define (evolve population cycles speed mutation rounds-per-match delta name-list)
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
         (out-rank cycles population (second name-list)))
    (if (zero? cycles)
        (begin
          (plot-mean population-mean (third name-list))
          (out-mean population-mean (first name-list))
          population)
        (evolve new-population (sub1 cycles) speed mutation rounds-per-match delta name-list))))


;; run mass

(define (run-one s r d)
  (define B (create-population 100))
  (define name-list (n->srd s r d))
  (define B1 (evolve B 100 s 1 r d name-list))
  (print "hi"))


(define (run-many list-of-speeds list-of-rounds list-of-deltas)
  (for* ([i (in-list list-of-speeds)]
         [j (in-list list-of-rounds)]
         [k (in-list list-of-deltas)])
    (run-one i j k)
    (set! population-mean (list 0))))
