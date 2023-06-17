#lang racket

;;;
;;; KNIGHT'S TOUR
;;
;; The goal of this game is to move th knight to every square on the 
;; board without landing on any square more than once.
;;;

(require 2htdp/universe
         lang/posn
         2htdp/image
         utils/list
         utils/2htdp/clicker)

;; The world state consists of:
;; - a flag inidcating whether the instructions have been seen.
;; - kinght's position as a natural number representing the squares
;;   as numbered from 0 to 63 going left to right, from top to bottom
;;   of the board.
;; - a list of legal next moves.
;; a list of squares the knight has already traversed.
;; - the clicker containers (encapsulating buttons and labels).
(struct world (inst-seen? pos legals tour containers) #:mutable #:transparent)

;;;
;;; KNIGHT MOVEMENT
;;;

;; Moves the knight from its current pos to the new pos
;; indicated by the values of container/button arguments
;; passed from the mouse-up click event. The move must
;; be valid in terms of being a valid, non-repeated knight
;; move.
(define (move c b ws x-pos y-pos)  
  (define cname (container-name c))
  (define bname (button-name b))
  
  ;; Calculate knight's new position and set the button's label appropriately.
  (define new-pos (+ (* 8 cname) bname))
  
  (when (member new-pos (world-legals ws))
    ;; Set the legals for the new position.
    (set-world-legals! ws (legals-for new-pos (world-tour ws)))
    
    ;; Get prev knight position.
    (define prev-pos (world-pos ws))
    (define prev-c/b (find-container/button (quotient prev-pos 8)
                                            (modulo prev-pos 8)
                                            (world-containers ws)))
    (define prev-c (first prev-c/b))
    (define prev-b (second prev-c/b))
    (define prev-cname  (container-name prev-c))
    (define prev-bname (button-name prev-b))
  
    (set-world-pos! ws new-pos)
    (set-button-label! b (get-button-label-for new-pos cname bname new-pos (world-tour ws)))

    ;; Set the old position's label appropriately.
    (set-button-label! prev-b (get-button-label-for prev-pos prev-cname prev-bname new-pos
                                                    (world-tour ws)))
    (set-world-tour! ws (cons new-pos (world-tour ws)))
    ws))

;; Returns a list of the legal squares for a knight
;; move from pos.
(define (legals-for pos tour)
  (define-values (cname bname) (quotient/remainder pos 8))  
  (define-values (r c) (quotient/remainder pos 8))
  (sort (remove* tour
                 (for/fold ([acc empty])
                           ([Δr '(-2 -2 -1 -1 +1 +1 +2 +2)]
                            [Δc '(-1 +1 -2 +2 -2 +2 -1 +1)])
                   (define r′ (+ r Δr))
                   (define c′ (+ c Δc))
                   (define n (+ (* 8 r′) c′))
                   (if (and (<= 0 r′ 7) (<= 0 c′ 7))
                       (cons n acc)
                       acc))) <))

;; This function determines what label a chess square is
;; to have: white, black, knight-on-white, knight-on-black,
;; or a previously traversed (tour) square.
(define (get-button-label-for n cname bname pos tour)
  (cond
    [(and (not (= n pos)) (member n tour)) TOUR-SQ]
    [(even? (+ cname bname))
     (if (= n pos)
         KNIGHT-ON-WHITE-SQ
         WHITE-SQ)]
    [else
     (if (= n pos)
         KNIGHT-ON-BLACK-SQ
         BLACK-SQ)]))


;;;
;;; Clicker parameters
;;;

;; Label defaults
(current-label-border? #f)

;; Button defaults
(current-button-up-action move)

;; Container defaults
(current-container-button-width 60)
(current-container-button-height (current-container-button-width))

;;;
;;; EVENT HANDLING
;;;

;; The puzzle responds to keys for beginning a new game
;; and for taking back a knight move.
(define (key-handler ws ke)
  (cond
    [(key=? ke " ") (new-world ws)]
    [(and (key=? ke "\b") (world-inst-seen? ws) (> (length (world-tour ws)) 1))
     (new-world ws (second (world-tour ws)) (drop (world-tour ws) 2))]
    [else (void)])
  (printf "~%~a~%" ws)
  ws)

;; The puzzle only responds to mouse-up clicks when:
;; - instructions have been seen
;; - puzzle is still solvable 
(define (mouse-handler ws x y evt)
  (unless (or (not (world-inst-seen? ws)) (solved? ws) (unsolvable? ws))
    (process-containers (world-containers ws) ws x y evt))  
  ws)

;; The puzzle is solved when all the squares have been traversed.
(define (solved? ws) (equal? (range 64) (world-tour ws)))

;; The puzzle is unsolvable when the puzzle hasn't been solved
;; and their are no legal moves left.
(define (unsolvable? ws) (and (not (solved? ws)) (empty? (world-legals ws))))

;; Builds the initial world state to be passed into big-bang. Instructions have
;; not been seen, the knight is position on square 0, legal moves and tour aare
;; empty, and the Clicker containers list is initialized with the board structure,
;; but labels are given default values only. 
(define (init-world)
  (define pos -1)
  (define tour (list pos))
  ;; Updates the containers list used by clicker functions.
  ;; This builds the container, button, and label configuraiton
  ;; in a framework state, which is not used except for the basis
  ;; of button assignmennt by new-world.
  (define containers
    (for/list ([n (range 64)]) 
      (define-values (c b) (quotient/remainder n 8))
      (make-container (name c)
                      (x-offset NOTATION-SIZE)
                      (y-offset (+ NOTATION-SIZE (* c (current-container-button-height))))
                      (buttons (for/list ([b (range 8)])
                                 (make-button (name b)
                                              (label (make-label))))))))
  (define ws (world #f pos empty tour containers))
  (new-world ws pos)
  (set-world-inst-seen?! ws #f)
  ws)

;; This is called each time SPACEBAR is pressed. It:
;; - marks the instructions as seen.
;; - assigns the knight's position as specified or randomly.
;; - assigns the tour list, adding knight pos to the tour argument.
;; - assigns legals computed for the knight's position.
;; - updates the board squares' labels.
(define (new-world ws (pos #f) (tour '()))
  (set-world-inst-seen?! ws #t)
  (set-world-pos! ws (if (false? pos) (random 64) pos))
  (set-world-tour! ws (cons (world-pos ws) tour))
  (set-world-legals! ws (legals-for (world-pos ws) (world-tour ws)))
  (for  ([n (range 64)])    
    (define-values (cname bname) (quotient/remainder n 8))
    (define c/b (find-container/button cname bname (world-containers ws)))
    (define c (first c/b))
    (define b (second c/b))    
    (set-button-label! b (get-button-label-for n cname bname (world-pos ws) (world-tour ws)))))

;;;
;;; RENDERING
;;;

(define WHITE-SQ (make-label (bg-color 'white)))
(define BLACK-SQ (make-label (bg-color 'black)))
(define TOUR-SQ (make-label (bg-color 'red)))

(define KNIGHT-FONT-SIZE 10)
(define KNIGHT-FONT-COLOR 'darkgray)

(define KNIGHT/BLACK-BG (overlay (text "\u265E" KNIGHT-FONT-SIZE KNIGHT-FONT-COLOR)
                                 (text "\u265E" (+ KNIGHT-FONT-SIZE 1) 'black)
                                 (text "\u265E" (+ KNIGHT-FONT-SIZE 2) 'black)))
(define KNIGHT/WHITE-BG (overlay (text "\u265E" KNIGHT-FONT-SIZE KNIGHT-FONT-COLOR)
                                 (text "\u265E" (+ KNIGHT-FONT-SIZE 1) 'white)
                                 (text "\u265E" (+ KNIGHT-FONT-SIZE 2) 'white)))

(define KNIGHT-ON-WHITE-SQ (make-label (name KNIGHT/BLACK-BG)
                                       (font-size #f) (bg-color 'white)))
(define KNIGHT-ON-BLACK-SQ (make-label (name KNIGHT/WHITE-BG)
                                       (font-size #f) (bg-color 'black)))

(define (draw-tour tour)
  (define lst (group (for/list ([pos (reverse tour)]
                                [n (in-naturals 1)])
                       (define-values (rank file) (quotient/remainder pos 8))
                       (text (format "~a. \u265E~a~a " n
                                     (case file
                                       [(0) 'a]
                                       [(1) 'b]
                                       [(2) 'c]
                                       [(3) 'd]
                                       [(4) 'e]
                                       [(5) 'f]
                                       [(6) 'g]
                                       [(7) 'h])
                                     (- 8 rank))
                             16 'white))
                     8))
  (define imgs (for/list ([grp lst])
                 (if (= (length grp) 1)
                     (car grp)
                     (apply beside grp))))
  (case (length imgs)
    [(0) empty-image]
    [(1) (car imgs)]
    [else (apply above/align "left" imgs)]))

(define BOARD-WIDTH  (* 8 (current-container-button-width)))
(define BOARD-HEIGHT (* 8 (current-container-button-height)))
(define BOARD (empty-scene BOARD-WIDTH BOARD-HEIGHT 'black))

(define NOTATION-SIZE (quotient (image-width BOARD) 16))
(define NOTATION-COLOR 'darkblue)
(define NOTATION-FONT-COLOR 'white)
(define NOTATION-SQ (square NOTATION-SIZE 'solid NOTATION-COLOR))
(define NOTATION-TILE (beside NOTATION-SQ NOTATION-SQ))

(define H-NOTATION (beside NOTATION-SQ
                           (apply beside (for/list ([v '(a b c d e f g h)])
                                           (overlay (text (~a v)
                                                          (quotient (image-height NOTATION-SQ) 2)
                                                          NOTATION-FONT-COLOR)
                                                    NOTATION-TILE)))
                           NOTATION-SQ))
(define V-NOTATION (apply above (for/list ([v '(8 7 6 5 4 3 2 1)])
                                  (overlay (text (~a v)
                                                 (quotient (image-height NOTATION-SQ) 2)
                                                 NOTATION-FONT-COLOR)
                                           (rotate 90 NOTATION-TILE)))))

(define BAR-WIDTH 4)
(define BAR-HEIGHT (+ (* 2 NOTATION-SIZE) BOARD-HEIGHT))
(define BAR (beside (rectangle BAR-WIDTH BAR-HEIGHT 'solid 'white)
                    (rectangle (* 4 BAR-WIDTH) BAR-HEIGHT 'solid 'transparent)))

(define TOUR-WIDTH (+ 32 (image-width (draw-tour (range 26)))))
(define TOUR-INFO (empty-scene TOUR-WIDTH BOARD-HEIGHT 'black))


(define TOUR-BANNER-FONT-SIZE 30)
(define TOUR-BANNER-FONT-COLOR 'white)

(define TOUR-BANNER (let ([img (text "TOUR"
                                     TOUR-BANNER-FONT-SIZE
                                     TOUR-BANNER-FONT-COLOR)])
                      (overlay img
                               (rectangle TOUR-WIDTH
                                          (* 3 (image-height img))
                                          'solid 'transparent))))

(define MT-WIDTH (+ (* 2 NOTATION-SIZE) BOARD-WIDTH BAR-WIDTH TOUR-WIDTH))
(define MT-HEIGHT (+ (* 2 NOTATION-SIZE) BOARD-HEIGHT))
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))

(define INST-FONT-SIZE 40)
(define INST-FONT-COLOR 'white)
(define INST-EMPHASIS-COLOR 'gold)
(define INST
  (above/align "left"
               (text "Press SPACEBAR" INST-FONT-SIZE INST-FONT-COLOR)
               (text "to begin" INST-FONT-SIZE INST-FONT-COLOR)
               (text "a new game." INST-FONT-SIZE INST-EMPHASIS-COLOR)
               (square INST-FONT-SIZE 'solid 'transparent)
               (text "Press DELETE" INST-FONT-SIZE INST-FONT-COLOR)
               (text "to take back" INST-FONT-SIZE INST-FONT-COLOR)
               (text "a move." INST-FONT-SIZE INST-EMPHASIS-COLOR)))

(define (draw-tour-info ws)
  (beside BAR
          (overlay/align "center" "top"
                         (above/align "left"
                                      TOUR-BANNER
                                      (draw-tour (world-tour ws)))
                         TOUR-INFO)))

(define WON (let ([img (text "SOLVED!" 80 'darkred)])
               (rotate 45 (overlay img
                                   (rectangle (+ 40 (image-width img))
                                              (+ 4 (image-height img))
                                              'solid 'gold)))))
(define LOST (let ([img (text "UNSOLVABLE!" 80 'darkred)])
               (rotate 45 (overlay img
                                   (rectangle (+ 40 (image-width img))
                                              (+ 4 (image-height img))
                                              'solid 'gold)))))


(define (render ws)
  (define img (make-parameter #f))
  (img (place-containers (world-containers ws)
                                   MT))
  (img (place-image/align V-NOTATION 0 NOTATION-SIZE "left" "top" (img)))
  (img (place-image/align V-NOTATION
                          (+ NOTATION-SIZE (* (current-container-button-width) 8))
                          NOTATION-SIZE "left" "top" (img)))
  (img (place-image/align H-NOTATION 0 0 "left" "top" (img)))
  (img (place-image/align H-NOTATION
                          0 (+ NOTATION-SIZE (* (current-container-button-width) 8))
                          "left" "top" (img)))
  (img (place-image/align BAR
                          (+ (* 2 NOTATION-SIZE) (* (current-container-button-width) 8))
                          0 "left" "top" (img)))
  (if (world-inst-seen? ws)
      (img (place-image/align (draw-tour-info ws)
                          (+ (* 2 NOTATION-SIZE) BAR-WIDTH (* (current-container-button-width) 8))
                          0 "left" "top" (img)))
      (img (place-image/align INST
                          (+ (* 4 NOTATION-SIZE) BAR-WIDTH (* (current-container-button-width) 8))
                          (* 2 NOTATION-SIZE) "left" "top" (img))))
  (when (world-inst-seen? ws)
    (when (unsolvable? ws)
      (img (place-image/align LOST NOTATION-SIZE NOTATION-SIZE "left" "top" (img))))
    (when (unsolvable? ws)
      (img (place-image/align LOST NOTATION-SIZE NOTATION-SIZE "left" "top" (img)))))  
  (img))

;; Runs the program
(big-bang (init-world)
  (on-mouse mouse-handler)
  (on-key key-handler)
  (to-draw render)
  (name "KNIGHT'S TOUR"))
