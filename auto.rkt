#lang racket

(provide struct-out automaton
         struct-out state
         state-name
         state-result0
         state-result1
         state-result2

         automaton-states
         automaton-current-state

         generate-auto
         jump-to-state
         react
         update
         flatten-automaton
         make-automaton
         state-labels
         current-claim)

;; AUTOMATON
(struct state (name result0 result1 result2) #:transparent)
;; a state: name and many transition rules
(struct automaton (current-state states) #:transparent)
;; the machine itself: current state + states

;; when an event happens, the right action needs to be chosen
(define (filter-state an-auto)
  (list-ref
   (automaton-states an-auto)
   (automaton-current-state an-auto)))

;; output: claim, not state
(define (jump-to-state an-event an-auto)
  (let ([result-state (filter-state an-auto)])
    (cond [(= an-event 0) (state-result0 result-state)]
          [(= an-event 1) (state-result1 result-state)]
          [(= an-event 2) (state-result2 result-state)])))

(define (react an-event an-auto)
(state-name
(list-ref (automaton-states an-auto)
(jump-to-state an-event an-auto))))

(define (update old-auto new-state)
  (struct-copy automaton old-auto [current-state new-state]))

; generate random automaton (random current state, random result-state
; after each event
(define (generate-auto)
  (automaton (random 5)
             (list (state (random 3) (random 5) (random 5) (random 5))
                   (state (random 3) (random 5) (random 5) (random 5))
                   (state (random 3) (random 5) (random 5) (random 5))
                   (state (random 3) (random 5) (random 5) (random 5))
                   (state (random 3) (random 5) (random 5) (random 5)))))

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
                   (apply state (take-right a-list 4)))))

(define (state-labels automaton)
  (map (lambda (a-state)
         (state-name a-state))
       (automaton-states automaton)))

(define (current-claim automaton)
  (state-name
   (list-ref (automaton-states automaton)
             (automaton-current-state automaton))))
