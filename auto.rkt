#lang racket

(provide (all-defined-out))

;; AUTOMATON
(struct state (name result0 result1 result2) #:transparent)
;; a state: name and many transition rules
(struct automaton (current-state states) #:transparent)
;; the machine itself: current state + states
;; mutable data structure seems very hard to handle later on

(define (automaton-current-strat an-auto)
  (state-name (list-ref
               (automaton-states an-auto)
               (automaton-current-state an-auto))))

(define (create-automaton* init-state
                           state0 state1 state2 state3 state4
                           state5 state6 state7 state8 state9
                           result00 result01 result02
                           result10 result11 result12
                           result20 result21 result22
                           result30 result31 result32
                           result40 result41 result42
                           result50 result51 result52
                           result60 result61 result62
                           result70 result71 result72
                           result80 result81 result82
                           result90 result91 result92)
  (automaton init-state
             (list (state state0 result00 result01 result02)
                   (state state1 result10 result11 result12)
                   (state state2 result20 result21 result22)
                   (state state3 result30 result31 result32)
                   (state state4 result40 result41 result42)
                   (state state5 result50 result51 result52)
                   (state state6 result60 result61 result62)
                   (state state7 result70 result71 result72)
                   (state state8 result80 result81 result82)
                   (state state9 result90 result91 result92)
                   )))

(define (create-automaton)
  (create-automaton* (random 10) ;; number 10 here is 10 states
                     (random 3) (random 3) (random 3) (random 3) (random 3)
                     (random 3) (random 3) (random 3) (random 3) (random 3)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                                        ))


;; AUTOMATON BEHAVIOR
(define (jump-to-state an-event an-auto)
  (define result-state (list-ref
                        (automaton-states an-auto)
                        (automaton-current-state an-auto)))
  (cond [(= an-event 0) (state-result0 result-state)]
        [(= an-event 1) (state-result1 result-state)]
        [(= an-event 2) (state-result2 result-state)]))

(define (react an-event an-auto)
  (define states (automaton-states an-auto))
  (define next-state-id (jump-to-state an-event an-auto))
  (state-name (list-ref states next-state-id)))

(define (update old-auto new-state)
  (struct-copy automaton old-auto
               [current-state new-state]))


(define (flatten-state a-state)
  (map (lambda (f) (f a-state))
       (list
        state-name
        state-result0
        state-result1
        state-result2)))

(define (flatten-automaton an-auto)
  (flatten
   (append
    (list (automaton-current-state an-auto))
    (map flatten-state (automaton-states an-auto)))))
(define (make-automaton a-list)
  (automaton (first a-list)
             (list (apply state (take (drop a-list 1) 4))
                   (apply state (take (drop a-list 5) 4))
                   (apply state (take (drop a-list 9) 4))
                   (apply state (take (drop a-list 13) 4))
                   (apply state (take (drop a-list 17) 4))
                   (apply state (take (drop a-list 21) 4))
                   (apply state (take (drop a-list 25) 4))
                   (apply state (take (drop a-list 29) 4))
                   (apply state (take (drop a-list 33) 4))
                   (apply state (take-right a-list 4)))))

(define (state-labels automaton)
  (map (lambda (a-state)
         (state-name a-state))
       (automaton-states automaton)))
