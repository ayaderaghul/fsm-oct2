#lang racket
(require racket/gui/base)
(define big-frame (new frame% [label "GUI"]))

(define msg (new message% [parent big-frame]
                 [label "hello world"]))

(send big-frame show #t)
