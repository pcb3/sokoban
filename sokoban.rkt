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
(define MIN DELTA)
(define MAX (- (* SIZE SIZE) DELTA))

; gaphical constants
(define (SQUARE colour)
  (square SIZE "solid" colour))
(define BLOCK (SQUARE "Cornflowerblue"))
(define GOAL
  (circle DELTA "solid" "gold"))
(define PLAYER
  (circle DELTA "solid" "tomato"))
(define START-MSG (text "SOKOBAN!" (* SIZE 2) 'lightgray))
(define MSG1 (text "MOVE WITH THE DIRECTION KEYS"
                   16 'lightgray))
(define MSG2 (text "PUSH BLOCKS ONTO THE GOAL TO WIN"
                   16 'lightgray))
(define MSG3 (text "R = RESET, Q = QUIT AND SPACE = START"
                   16 'lightgray))
(define LAST-MSG (text "GAME OVER" (* SIZE 2) 'lightgray))

; structures
(define-struct board [player block goal])

; Interpretation

; a player is a Posn:
; a player is the user controllable object
(define PLAYER0 (make-posn DELTA DELTA))
(define PLAYER1 (make-posn (* 2 DELTA) (* 2 DELTA)))

; a block is one of:
; '()
; (cons Posn block)
; a block is a list of block positions on the board
(define BLOCK0 '())
(define BLOCK1
  (list (make-posn (* 3 DELTA) (* 3 DELTA))))
(define BLOCK2
  (list (make-posn (* 3 DELTA) (* 3 DELTA))
        (make-posn (* 4 DELTA) (* 4 DELTA))))

; a goal is a Posn:
; a goal is the position a block
; must inhabit in order to trigger the win condition
(define GOAL0 (make-posn DELTA DELTA))
(define GOAL1 (make-posn (* 5 DELTA) (* 5 DELTA)))

; a Board is a structure
; (make-state Player Block Goal)
; a Board is the state of the game, including
; player, block and goal positions
(define BOARD0 (make-board PLAYER0 BLOCK0 GOAL0))
(define BOARD1 (make-board PLAYER1 BLOCK2 GOAL1))
(define BOARD5 (make-board
                (make-posn MIN MIN)
                (list (make-posn (+ MIN SIZE) (+ MIN SIZE)))
                (make-posn (+ MIN (* SIZE 2)) MIN)))
(define START (make-board
               (make-posn -100 0)
               '()
               (make-posn -100 0)))
(define LEVEL1 (make-board
                (make-posn (/ WIDTH 2)
                           (/ HEIGHT 10))
                (list (make-posn (/ WIDTH 2)
                                 (/ HEIGHT 5)))
                (make-posn (/ WIDTH 2)
                           (/ HEIGHT 2))))
(define QUIT (make-board
              (make-posn -100 0)
              (list (make-posn -100 0))
              (make-posn -100 0)))

; Board -> Board
; consumes a Board b and produces a new Board
; updated each tick

(check-expect (tock BOARD0) BOARD0)

(define (fn-tock b)
  (make-board (board-player ...)
              (board-block ...)
              (board-goal ...)))

(define (tock b)
  (make-board (board-player b)
              (board-block b)
              (board-goal b)))

; Board Image -> Image
; consumes a Board b and image im and renders an
; image to the screen

(check-expect
 (render BOARD1)
 (place-image
  PLAYER
  (posn-x (board-player BOARD1))
  (posn-y (board-player BOARD1))
  (place-image
   BLOCK
   (posn-x (first (board-block BOARD1)))
   (posn-y (first (board-block BOARD1)))
   (place-image
    BLOCK
    (posn-x (first (rest (board-block BOARD1))))
    (posn-y (first (rest (board-block BOARD1))))
    (place-image
     GOAL
     (posn-x (board-goal BOARD1))
     (posn-y (board-goal BOARD1))
     MT)))))

(define (render b)
  (local (; Board Image -> Image
          ; consumes a Board b and image im and renders the
          ; player to the screen

          (define (fn-render-player b im)
            (place-image ...
             (posn-x (board-player ...))
             (posn-y (board-player ...))
             ...))

          (define (render-player b im)
            (place-image PLAYER
                         (posn-x (board-player b))
                         (posn-y (board-player b))
                         im))

          ; Board Image -> Image
          ; consumes a block blk and image im and renders the
          ; blocks to the screen

          (define (fn-render-block blk im)
            (cond
              [(empty? ...) ...]
              [else
               (place-image
                BLOCK
                (posn-x (first ...))
                (posn-y (first ...))
                (render-block (rest ...) ...))]))

          (define (render-block blk im)
            (cond
              [(empty? blk) im]
              [else
               (place-image
                BLOCK
                (posn-x (first blk))
                (posn-y (first blk))
                (render-block (rest blk) im))]))
      
          ; Board Image -> List Image
          ; consumes a Board b and Image im and extracts the
          ; list of blocks  

          (define (fn-extract-block b im)
            (render-block (board-block ...) ...))

          (define (extract-block b im)
            (render-block (board-block b) im))

          ; Board Image -> Image
          ; consumes a Board b and image im and renders the
          ; goal to the screen

          (define (fn-render-goal b im)
            (place-image ...
             (posn-x (board-goal ...))
             (posn-y (board-goal ...))
             ...))

          (define (render-goal b im)
            (place-image GOAL
                         (posn-x (board-goal b))
                         (posn-y (board-goal b))
                         im))

          ; Board -> Boolean
          ; consumes a Board b and returns true if its
          ; the initial setup

          (define (fn-start? b)
            (equal? ... ...))

          (define (start? b)
            (equal? b START))

          ; Image Image Image Image -> Image
          ; consumes four Images and produces the first screen

          (define (start-screen start msg1 msg2 msg3)
            (place-image start (/ WIDTH 2) (/ HEIGHT 6)
                         (place-image
                          msg1 (/ WIDTH 2) (/ HEIGHT 3)
                          (place-image
                           msg2 (/ WIDTH 2) (/ HEIGHT 2)
                           (place-image
                            msg3 (/ WIDTH 2) (/ HEIGHT 1.5) MT))))))
       
    (cond
      [(start? b)
       (start-screen START-MSG MSG1 MSG2 MSG3)]
      [else
       (render-player b
                      (extract-block b
                                     (render-goal b MT)))])))

; Board Key -> Board
; consumes a Board b and Key k and produces a new
; Board dependent on the key

(define (control b key)
  (local (; consumes a Board b and Key key and moves
          ; the player in given direction

          (define (move-player b key)
            (cond
              [(string=? key "left")
               (make-board
                (make-posn (- (posn-x (board-player b)) SIZE)
                           (posn-y (board-player b)))
                (board-block b) (board-goal b))]
              [(string=? key "right")
               (make-board
                (make-posn (+ (posn-x (board-player b)) SIZE)
                           (posn-y (board-player b)))
                (board-block b) (board-goal b))]
              [(string=? key "up")
               (make-board
                (make-posn (posn-x (board-player b))
                           (- (posn-y (board-player b)) SIZE))
                (board-block b) (board-goal b))]
              [(string=? key "down")
               (make-board
                (make-posn (posn-x (board-player b))
                           (+ (posn-y (board-player b)) SIZE))
                (board-block b) (board-goal b))]
              [else b]))
          

          ; consumes a Board b and Key key and produces
          ; a new list excluding the movable block

          (define (filter-blocks blocks)
            (filter player-block-equal? blocks))

          ; consumes two Posns player and block and produces true
          ; if they are not equal

          (define (player-block-equal? x)
            (cond
              [(string=? key "right")
               (cond
                 [(not (equal? (+ (posn-x (board-player b)) SIZE)
                               (posn-x x)))
                  #true]
                 [else #false])]
              [(string=? key "left")
               (cond
                 [(not (equal? (- (posn-x (board-player b)) SIZE)
                               (posn-x x)))
                  #true]
                 [else #false])]
              [(string=? key "up")
               (cond
                 [(not (equal? (- (posn-y (board-player b)) SIZE)
                               (posn-y x)))
                  #true]
                 [else #false])]
              [(string=? key "down")
               (cond
                 [(not (equal? (+ (posn-y (board-player b)) SIZE)
                               (posn-y x)))
                  #true]
                 [else #false])]
              [else #false]))
                       

          ; Board Key -> Boolean
          ; consumes a Board and Key key and produces true if the player
          ; is at a boundary

          (define (player-boundary? b key)
            (cond
              [(string=? key "right")
               (> (+ (posn-x (board-player b)) DELTA)
                  MAX)]
              [(string=? key "left")
               (< (- (posn-x (board-player b)) DELTA)
                  MIN)]
              [(string=? key "up")
               (< (- (posn-y (board-player b)) DELTA)
                  MIN)]
              [(string=? key "down")
               (> (+ (posn-y (board-player b)) DELTA)
                  MAX)]
              [else #false]))

          ; Board Key -> Board
          ; consumes a Board b and a Key key and produces a
          ; new board with updated Player and Block positions

          (define (move-block b key)
            (cond
              [(string=? key "left")
               (make-board
                (make-posn (- (posn-x (board-player b)) SIZE)
                           (posn-y (board-player b)))
                (cons
                 (make-posn (- (posn-x (board-player b)) (* 2 SIZE))
                            (posn-y (board-player b)))
                 (filter-blocks (board-block b)))
                (board-goal b))]
              [(string=? key "right")
               (make-board
                (make-posn (+ (posn-x (board-player b)) SIZE)
                           (posn-y (board-player b)))
                (cons
                 (make-posn (+ (posn-x (board-player b)) (* 2 SIZE))
                            (posn-y (board-player b)))
                 (filter-blocks (board-block b)))
                (board-goal b))]
              [(string=? key "up")
               (make-board
                (make-posn (posn-x (board-player b))
                           (- (posn-y (board-player b)) SIZE))
                (cons
                 (make-posn (posn-x (board-player b))
                            (- (posn-y (board-player b)) (* 2 SIZE)))
                 (filter-blocks (board-block b)))
                (board-goal b))]
              [(string=? key "down")
               (make-board
                (make-posn (posn-x (board-player b))
                           (+ (posn-y (board-player b)) SIZE))
                (cons
                 (make-posn (posn-x (board-player b))
                            (+ (posn-y (board-player b)) (* 2 SIZE)))
                 (filter-blocks (board-block b)))
                (board-goal b))]
              [else b]))

          ; Board Key -> Boolean
          ; consumes a Board b and a Key key and produces true if the
          ; player has a free space to move into

          (define (move-player-space? b key)
            (cond
              [(and (string=? key "right")
                    (not (member? (make-posn
                                   (+ (posn-x (board-player b)) SIZE)
                                   (posn-y (board-player b)))
                                  (board-block b))))
               #true]
              [(and (string=? key "left")
                    (not (member? (make-posn
                                   (- (posn-x (board-player b)) SIZE)
                                   (posn-y (board-player b)))
                                  (board-block b))))
               #true]
              [(and (string=? key "up")
                    (not (member? (make-posn
                                   (posn-x (board-player b))
                                   (- (posn-y (board-player b)) SIZE))
                                  (board-block b))))
               #true]
              [(and (string=? key "down")
                    (not (member? (make-posn
                                   (posn-x (board-player b))
                                   (+ (posn-y (board-player b)) SIZE))
                                  (board-block b))))
               #true]
              [else
               #false]))

          ; Key -> Boolean
          ; consumes a Key key and returns true if "q" or "Q" is pressed

          (define (fn-quit? key)
            (or (string=? key ...)
                (string=? key ...)))

          (define (quit? key)
            (or (string=? key "q")
                (string=? key "Q")))

          ; Board -> Boolean
          ; consumes a Board b and returns true if it is the first screen

          (define (fn-start? b)
            (equal? ... ...))

          (define (start? b)
            (equal? b START))

          ; Key -> Boolean
          ; consumes a Key key and returns true if the the space
          ; key has been pressed

          (define (fn-space? key)
            (string=? key ...))

          (define (space? key)
            (string=? key " "))

          ; Key -> Boolean
          ; consumes a Key key and returns true if the "r" or "R" key
          ; is pressed

          (define (fn-reset? key)
            (or (string=? key ...)
                (string=? key ...)))

          (define (reset? key)
            (or (string=? key "r")
                (string=? key "R"))))
                  
    (cond
      [(quit? key)
       QUIT]
      [(and (start? b) (not (space? key)))
       START]
      [(and (start? b) (space? key))
       LEVEL1]
      [(reset? key)
       LEVEL1]
      [(push? b key)
       (move-block b key)]
      [(and (move-player-space? b key)
            (not (player-boundary? b key)))
       (move-player b key)]
      [else b])))

; Board -> Boolean
; consumes a Board b and produces true if the end
; condition has been met

(check-expect (last-world? BOARD0) #false)

(check-expect
 (last-world? (make-board
               (board-player BOARD1)
               (list
                (make-posn SIZE SIZE))
               (make-posn SIZE SIZE))) #true)

(define (fn-last-world? b)
  (member? (board-goal ...)
           (board-block ...)))

(define (last-world? b)
  (member? (board-goal b)
           (board-block b)))

; Board -> Image
; consumes a Board b and produces the final image
(check-expect
 (last-picture BOARD1)
 (place-image LAST-MSG (/ WIDTH 2) (/ HEIGHT 2)
              (render BOARD1)))

(define (fn-last-picture b)
  (place-image ... ... ...
               (render ...)))

(define (last-picture b)
  (place-image LAST-MSG (/ WIDTH 2) (/ HEIGHT 2)
               (render b)))

; Board Key -> Boolean
; consumes a Board b and a Key key and returns
; true if its possible to push an adjacent block

(check-expect
 (push?
  (make-board
   (make-posn MIN MIN)
   (list (make-posn (+ MIN SIZE) MIN))
   (make-posn (+ MIN (* SIZE 2)) MIN)) "right")
 #true)

(check-expect
 (push?
  (make-board
   (make-posn MIN MIN)
   (list (make-posn (+ MIN SIZE) MIN))
   (make-posn (+ MIN (* SIZE 2)) MIN)) "left")
 #false)

(define (push? b key)
  (local  (; Board Key -> Boolean
           ; consumes a Board b and Key key and
           ; produces true if the players coord
           ; is a member of any of the blocks coord
           ; in the respective axis
           
           (define (adjacent? b key)
             (cond
               [(and (string=? key "right")
                     (member?
                      (make-posn (+ (posn-x (board-player b)) SIZE)
                                 (posn-y (board-player b)))
                      (board-block b)))
                #true]
               [(and (string=? key "left")
                     (member?
                      (make-posn (- (posn-x (board-player b)) SIZE)
                                 (posn-y (board-player b)))
                      (board-block b)))
                #true]
               [(and (string=? key "up")
                     (member?
                      (make-posn (posn-x (board-player b))
                                 (- (posn-y (board-player b)) SIZE))
                      (board-block b)))
                #true]
               [(and (string=? key "down")
                     (member?
                      (make-posn (posn-x (board-player b))
                                 (+ (posn-y (board-player b)) SIZE))
                      (board-block b)))
                #true]
               [else
                #false]))

           ; Board key -> Boolean
           ; consumes a Board b and Key key and returns true if
           ; there is a block adjacent the block to be pushed         

           (define (blocked? b key)
             (cond
               [(string=? key "right")
                (cond
                  [else
                   (member?
                    (make-posn (+ (posn-x (board-player b))
                                  (* 2 SIZE))
                               (posn-y (board-player b)))
                    (board-block b))])]
               [(string=? key "left")
                (cond
                  [else
                   (member?
                    (make-posn (- (posn-x (board-player b))
                                  (* 2 SIZE))
                               (posn-y (board-player b)))
                    (board-block b))])]
               [(string=? key "up")
                (cond
                  [else
                   (member?
                    (make-posn (posn-x (board-player b))
                               (- (posn-y (board-player b))
                                  (* 2 SIZE)))
                    (board-block b))])]
               [(string=? key "down")
                (cond
                  [else
                   (member?
                    (make-posn (posn-x (board-player b))
                               (+ (posn-y (board-player b))
                                  (* 2 SIZE)))
                    (board-block b))])]
               [else #false]))

           ; Board Key -> Boolean
           ; consumes a Board b and a Key key and returns
           ; true if there is a boundary in the direction
           ; of intended movement

           (define (boundary? b key)
             (cond
               [(string=? key "right")
                (> (+ (posn-x (board-player b))
                      (* 2 SIZE))
                   MAX)]
               [(string=? key "left")
                (< (- (posn-x (board-player b))
                      (* 2 SIZE))
                   MIN)]
               [(string=? key "up")
                (< (- (posn-y (board-player b))
                      (* 2 SIZE))
                   MIN)]
               [(string=? key "down")
                (> (+ (posn-y (board-player b))
                      (* 2 SIZE))
                   MAX)]
               [else #false])))
    
    (and (adjacent? b key)
         (not (blocked? b key))
         (not (boundary? b key)))))

; Board -> Board 
; launches the program from some initial state b

(define (sokoban rate b)
  (big-bang b
    [on-tick tock rate]
    [to-draw render]
    [on-key control]
    [stop-when last-world? last-picture]
    [state #f]
    [close-on-stop 3]
    [name "Sokoban"]
    ))

; usage
(sokoban 1 START)



























