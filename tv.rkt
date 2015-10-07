#lang racket
(provide plot-mean
	plot-dynamic)

(require plot)
(plot-new-window? #t)

;; TV


(define (generate-plot-png delta)
  (string-append "simuI/simu"
                 (string-trim (number->string (* 10 delta)) ".0")
                 "/plot.png"))

(define (plot-mean data delta)
  (let* ([l (length data)]
         [coors (map list (build-list l values)
                     data)])
    (plot (lines coors)
          #:y-min 0
	 #:out-file (generate-plot-png delta)
          #:width 600)
    ))

(define (plot-dynamic data)
  (plot (lines data)
        #:x-min 0 #:x-max 100
        #:y-min 0 #:y-max 100))
#|
(define dynamic-frame (new frame%
                           [label "dynamic"]
                           [width 400]
                           [height 400]))
(define dynamic-canvas (new canvas%
                            [parent dynamic-frame]))
(define dynamic-dc (send dynamic-canvas get-dc))
|#
