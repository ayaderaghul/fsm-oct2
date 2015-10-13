#lang racket
(require "auto.rkt")
(require "csv.rkt")
(require "scan.rkt")
(provide out-data
         out-mean
         out-rank
         n->srd
         chop
         make-body
         make-labels
         generate-codes
         export-codes
         )

;; data:
;; '((1 2..)
;;   (2 3..))

;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))


(define (out-mean data mean-file)
  (out-data mean-file (map list data)))

(define (out-rank day population n rank-file)
  [define ranking (rank population)]
  [define l (length ranking)]
  (out-data rank-file (append (list (list day))
                              (map list
                                   (if (<= l n)
                                       ranking
                                       (take (rank population) n)
                                   )))))

(define (n->srd s r d)
  (let ([pre-name (string-append
                   ;"R:/fsm-oct2/"
                   "s" (number->string s) "r" (number->string r)
                   "d" (string-trim (number->string (* 10 d)) ".0"))])
    (list
     (string-append pre-name "mean.txt")
     (string-append pre-name "rank.txt")
     (string-append pre-name "plot.png"))))



(define (chop automaton)
  (let ([states (automaton-states automaton)])
    (map (lambda (a-state)
           (drop (vector->list (struct->vector a-state)) 2))
         states)))

(define (convert-end-points state-ends)
  (let ([a (first state-ends)]
        [b (second state-ends)]
        [c (third state-ends)])
    (cond
     [(= a b c) (list "L,M,H" "L,M,H" "L,M,H")]
     [(= a b) (list "L,M" "L,M" "H")]
     [(= a c) (list "L,H" "M" "L,H")]
     [(= b c) (list "L" "M,H" "M,H")]
     [else (list "L" "M" "H")])))


(define (collect-end-points automaton)
  (flatten
   (for/list ([i 5])
     (convert-end-points (list-ref (chop automaton) i)))))

(define (make-body automaton)
  (let ([end-points (flatten (chop automaton))]
        [end-labels (collect-end-points automaton)])
    (list*
     "Graph[{-1 -> ~a"
     (remove-duplicates
      (for/list ([i 15])
        (format
         (list-ref trajectories i)
         (list-ref end-points i)
         (list-ref end-labels i))))
     )))

;; export matha code

(define trajectories
  (list
   "Labeled[0 -> ~a, ~s]" "Labeled[0 -> ~a, ~s]" "Labeled[0 -> ~a, ~s]"
   "Labeled[1 -> ~a, ~s]" "Labeled[1 -> ~a, ~s]" "Labeled[1 -> ~a, ~s]"
   "Labeled[2 -> ~a, ~s]" "Labeled[2 -> ~a, ~s]" "Labeled[2 -> ~a, ~s]"
   "Labeled[3 -> ~a, ~s]" "Labeled[3 -> ~a, ~s]" "Labeled[3 -> ~a, ~s]"
   "Labeled[4 -> ~a, ~s]" "Labeled[4 -> ~a, ~s]" "Labeled[4 -> ~a, ~s]"))

(define labels
  (list
   "0 -> Placed[~s, Center]"
   "1 -> Placed[~s, Center]"
   "2 -> Placed[~s, Center]"
   "3 -> Placed[~s, Center]"
   "4 -> Placed[~s, Center]"))

(define (labelise num)
  (cond [(zero? num) "L"]
        [(= num 1) "M"]
        [(= num 2) "H"]))

(define (make-labels automaton)
  (let ([inputs (map labelise (state-labels automaton))])
    (for/list ([i 5])
      (format
       (list-ref labels i)
       (list-ref inputs i)))))

(define (generate-code a-list posn x)
  (let ([automaton (list-ref a-list posn)])
    (format
     (string-append*
      (append
       (list "VertexCircle[{xc_, yc_}, name_, {w_, h_}] := Disk[{xc, yc}, .1];
")
       (list "~aGraph =
")
       (cdr
        (append*
         (map (lambda (x) (list ", " x))
              (make-body automaton))))
       (list "},
   EdgeShapeFunction -> GraphElementData[\"EdgeShapeFunction\", \"FilledArrow\"],
   VertexStyle -> LightGray,
   VertexShapeFunction -> VertexCircle,
   VertexLabels -> {")

       (cdr
        (append*
         (map (lambda (x) (list ", " x))
              (make-labels automaton))))
       (list
        "}
  ];
")
       (list
        "Export[\"~a.png\", ~aGraph];
"
        )))
     (name x posn)
     (automaton-current-state automaton)
     (name x posn)
     (name x posn))))


(define (generate-codes a-list x)
  (for/list ([i (length a-list)])
    (generate-code a-list i x)))

(define (export-codes a-list x)
  (for ([i (length a-list)])
    (with-output-to-file "auto-code"
      (lambda () (printf (generate-code a-list i x)))
      #:exists 'append)))

(define (name x n)
  (string->symbol (string-append x (number->string n))))
