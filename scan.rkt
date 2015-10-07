#lang racket
(require "auto.rkt")
(require "match.rkt")
(provide scan-init
	scan
	scan-types
        rank
        top
        contest)

;; SCAN
(define (scan-init population)
  (foldl
   (lambda (au h)
     (hash-update h
                  (state-name (list-ref
                               (automaton-states au)
                               (automaton-current-state au)))
                  add1 0))
   (hash)
   population))

(define (scan population)
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   population))

(define (scan-flattened population)
  (let ([flattened (map flatten-automaton population)])
    (foldl
     (lambda (au h)
       (hash-update h au add1 0))
   (hash)
   flattened)))

(define (rank population)
  (let ([ranking (hash->list (scan-flattened population))])
    (sort ranking > #:key cdr)))

  (define (hash-ref* a-hash a-key)
  (if (hash-has-key? a-hash a-key)
      (hash-ref a-hash a-key)
      0))
(define (scan-types population)
  (let ([types (scan-init population)])
    (list
     (hash-ref* types 0)
     (hash-ref* types 1))))

;; TOP
(define (top t population)
  (let* ([flattened (map car (rank population))]
         [automaton (map make-automaton (take flattened t))])
    (for/list ([i t])
      (eval
       (list 'define (x->ax i)
             (list-ref automaton i))))))

(define (x->ax x)
  (string->symbol (string-append "a" (number->string x))))

(define (generate-ax a-list)
  (map x->ax a-list))

(define (contest an-auto a-list)
  (for/list ([n (length a-list)])
    (match-pair (list an-auto (list-ref a-list n)) 10)))
