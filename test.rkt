#lang racket

(require "auto.rkt")

(provide create-test-population)

;; create test population

(define (create-test-population l m h)
  (shuffle
   (append
    (for/list
        ([n l])
      (generate-lows))
    (for/list
        ([n m])
      (generate-mediums))
    (for/list
        ([n h])
      (generate-highs)))))

(define (generate-lows)
  (make-automaton
   (list 0
         0 (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5))))


(define (generate-mediums)
  (make-automaton
   (list 0
         1 (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5))))


(define (generate-highs)
  (make-automaton
   (list 0
         2 (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5))))
