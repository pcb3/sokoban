;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sokoban) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; pcb3 https://github.com/pcb3/sokoban

; Sokoban

(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define SIZE 20)
(define DELTA (/ SIZE 2))
(define WIDTH (* SIZE SIZE))
(define HEIGHT (* SIZE SIZE))
(define MT (empty-scene WIDTH HEIGHT))
(define MAX (* SIZE SIZE))

; gaphical constants
(define (SQUARE colour)
  (square SIZE "solid" colour))
(define BLOCK (SQUARE "Cornflowerblue"))
(define GOAL
  (circle DELTA "solid" "gold"))
(define PLAYER
  (circle DELTA "solid" "tomato"))

; structures
(define-struct board [player block goal])

; Interpretation

; a player is a Posn:
; a player is the user controllable object
(define PLAYER0 (make-posn DELTA DELTA))

; a block is one of:
; '()
; (cons Posn block)
; a block is a list of block positions on the board
(define BLOCK0
  (list (make-posn (* 1 DELTA) (* 2 DELTA))
        (make-posn (* 1 DELTA) (* 3 DELTA))))

; a goal is a Posn:
; a goal is the position the the position a block
; must inhabit in order to trigger the win condition
(define GOAL0 (make-posn (* DELTA 3) (* DELTA 3)))

; a board is a structure
; (make-state Player Block Goal)
; a board is the state of the game, including
; player, block and goal positions
(define BOARD0 (make-board PLAYER0 BLOCK0 GOAL0))

; board -> board 
; launches the program from some initial state c

;(define (sokoban rate)
  ;(big-bang BOARD0
    ;[on-tick tock rate]
    ;[to-draw render]
    ;[stop-when last-world? last-picture]
    ;[state #t]
    ;[close-on-stop 3]
    ;[name "Sokoban"]
    ;))

; usage
;(sokoban 0.1)


