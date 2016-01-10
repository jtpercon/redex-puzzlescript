#lang racket
(require redex)
(require "redex-puzzlescript.rkt")

; As a first example for redex-puzzlescript,
; I'm re-implementing David Skinner's Microban
; from the PuzzleScript editor's built-in sample games

(define preamble
  (term ("title Microban"
         "author David Skinner"
         "homepage www.sneezingtiger.com/sokoban/levels/microbanText.html")))

; tiles

(define-tiles background
  (lightgreen green)
  #<<>
11111
01111
11101
11111
10111
>
  )