;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sokoban) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; pcb3 https://github.com/pcb3/sokoban
; Sokoban

(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define SIZE 20)
(define WIDTH (* SIZE SIZE))
(define HEIGHT (* SIZE SIZE))
(define MT (empty-scene WIDTH HEIGHT))
(define MAX (* SIZE SIZE))

; gaphical constants
(define (BLOCK colour)
  (square SIZE "solid" colour))
(define MBLOCK (BLOCK "tomato"))
(define FBLOCK (BLOCK "Cornflowerblue"))
(define GOAL
  (circle (/ SIZE 2) "solid" "gold"))

; Permutation -> Permutation
; launches the program from some initial state c

;(define (sokoban rate)
  ;(big-bang STATE0
    ;[on-tick tock rate]
    ;[to-draw render]
    ;[stop-when last-world? last-picture]
    ;[state #t]
    ;[close-on-stop 3]
    ;[name "Sokoban"]
    ;))

; usage
;(sokoban 0.1)


