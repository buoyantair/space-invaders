;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Space invaders clone!

;; =================
;; Constants:

(define SCREENW 300)
(define SCREENH 600)
(define MTS (empty-scene SCREENW SCREENH))
(define TANK (square 50 "solid" "green"))
(define FOE (square 25 "solid" "yellow"))
(define PROJ (rectangle 5 10 "solid" "red"))
(define TANK-VEL 2)
(define FOE-SWNG 5)
(define FOE-VEL 3)
(define TANK-Y (- SCREENH (/ (image-height TANK) 2)))
(define BLANK (square 0 "solid" "white"))
(define PROJ-VEL 10)

;; =================
;; Data definitions:

(define-struct proj (x y))
;; Projectile is (make-proj Number Number)
;; interp. as the position of projectile in the game.
(define P1 (make-proj 34 4))
(define P1x (make-proj 34 (+ 4 PROJ-VEL)))
(define P2 (make-proj 43 2))
(define P2x (make-proj 43 (+ 2 PROJ-VEL)))
(define P3 (make-proj 34 0))
(define P4 (make-proj 43 34))
#;
(define (fn-for-proj p)
  (... (proj-x p)
       (proj-y p)))

;; Template rules used:
;;  atomic: 2 fields



(define-struct tank (x y dx))
;; Tank is (make-tank Number Number Integer)
;; interp. as a tank that can move along the horizontal axis with some velocity.
(define T1 (make-tank 0 TANK-Y TANK-VEL))
(define T1x (make-tank (+ 0 TANK-VEL) TANK-Y TANK-VEL))
(define T2 (make-tank (/ SCREENW 2) TANK-Y TANK-VEL))
(define T2x (make-tank (+ (/ SCREENW 2) TANK-VEL) TANK-Y TANK-VEL))

(define T3 (make-tank (/ SCREENW 4) TANK-Y TANK-VEL))
(define T3x (make-tank (/ SCREENW 4) TANK-Y TANK-VEL))
(define T4 (make-tank (/ SCREENW 4) TANK-Y TANK-VEL))
(define T4x (make-tank (/ SCREENW 4) TANK-Y (- TANK-VEL)))
#;
(define (fn-for-t t)
  (... (tank-x t)
       (tank-y t)))

;; Template rules used:
;;  compound: 2 fields

(define-struct foe (x y dx))
;; Foe is (make-foe Number Number Integer)
;; interp as a foe at the point (x, y) with the tendency to move with dx velocity.

(define F1 (make-foe (/ SCREENW 2) 0 1))
(define F1x (make-foe (+ (/ SCREENW 2) FOE-SWNG) (+ 0 FOE-VEL) 1))
(define F2 (make-foe 43 34 -4))
(define F2x (make-foe (+ 43 FOE-SWNG) (+ 34 FOE-VEL) -4))
(define F3 (make-foe 50 (/ SCREENH 2) 0))
(define F4 (make-foe 43 SCREENH 0))
(define F5 (make-foe SCREENW 34 0))
(define F6 (make-foe 0 34 0))
(define F7 (make-foe -1 4 0))
(define F8 (make-foe (+ SCREENW 1) 34 0))


#;
(define (fn-for-foe f)
  (... (foe-x f)
       (foe-y f)
       (foe-dx f)))

;; Template rules used:
;;  - compound: 3 fields.


;; ListOfFoe is one of:
;;  - empty
;;  - Foe
;; interp. as a list of foes.

(define LOF1 empty)
(define LOF1x empty)
(define LOF2 (list F1 F2))
(define LOF3 (list F1 F2 F3 F4))
(define LOF2x (list F1x F2x))

#;
(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
        [else
         (... (first lof)
              (fn-for-lof (rest lof)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Foe ListOfFoe)
;;  - self-reference: (rest lof) is ListOfFoe
;;  - reference: (first lof) is Foe

;; ListOfProj is one of:
;;  - empty
;;  - Proj
;; interp. as a list of Projectiles.

(define LOP1 empty)
(define LOP1x empty)
(define LOP2 (list P1 P2))
(define LOP2x (list P1x P2x))
(define LOP3 (list P1 P2 P3))
(define LOP4 (list P1 P2 (make-proj 43 34)))
#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (first lop)
              (fn-for-lop (rest lop)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Proj ListOfProj)
;;  - self-reference: (rest lop) is ListOfProj
;;  - reference: (first lop) is Proj


(define-struct game (foes tank projs))
;; Game is (make-game ListOfFoes Tank ListOfProj)
;; interp. as a structure containing a list of foes and a tank and a list of projectiles.

(define G1 (make-game LOF1 T1 LOP1))
(define G1x (make-game LOF1x T1x LOP1x))
(define G2 (make-game LOF2 T2 LOP2))
(define G2x (make-game LOF2x T2x LOP2x))
(define G3 (make-game LOF3 T2 LOP2x))

#;
(define (fn-for-game g)
  (... (fn-for-lof (game-foes g))
       (fn-for-t (game-tank g))
       (fn-for-lop (game-projs g))))


 
;; =================
;; Functions:

;; ListOfProj Tank-> ListOfProj
;; insert a new projectile at the given tank's position
(check-expect (insert-proj LOP1 T1)
              (append
               (list (make-proj (tank-x T1) (tank-y T1)))
               LOP1))
(define (insert-proj lop t)
  (append
   (list (make-proj (tank-x t) (tank-y t)))
   lop))

;; ListOfProj -> Image
;; place images in MTS from the list
(check-expect (posn-projs LOP1) empty)
(check-expect (posn-projs LOP2)
              (list (make-posn (proj-x P1) (proj-y P1))
                    (make-posn (proj-x P2) (proj-y P2))))
(define (posn-projs lop)
  (cond [(empty? lop) empty]
        [else
         (append 
          (if (empty? (rest lop))
              (list (make-posn (proj-x (first lop)) (proj-y (first lop))))
              (cons (make-posn (proj-x (first lop)) (proj-y (first lop)))
                    (posn-projs (rest lop)))))]))


;; Image ListOfProj -> ListOfImage
;; produce a list of images from the consumed list
(check-expect (repeat-projs LOP1) empty)
(check-expect (repeat-projs LOP2)
              (list PROJ PROJ))
(check-expect (repeat-projs LOP3)
              (list PROJ PROJ PROJ))
(define (repeat-projs lop)
  (cond [(empty? lop) empty]
        [else
         (append (if (empty? (rest lop))
                     (list PROJ)
                     (cons PROJ
                           (repeat-projs (rest lop)))))]))


;; Image ListOfFoe -> ListOfImage
;; produce a list of images from the consumed list
(check-expect (repeat-foes LOF1) empty)
(check-expect (repeat-foes LOF2)
              (list FOE FOE))
(define (repeat-foes lof)
  (cond [(empty? lof) empty]
        [else
         (if (empty? (rest lof))
             (list FOE)
             (cons FOE
                   (repeat-foes (rest lof))))]))

;; Tank -> Position
;; produce point (x, y)
(check-expect (posn-tank T1) (make-posn (tank-x T1) (tank-y T1)))
(define (posn-tank t)
  (make-posn (tank-x t) (tank-y t)))


;; Foe -> Boolean
;; produce true if the foe has touched the ground
(check-expect (grounded? F1) false)
(check-expect (grounded? F4) true)
(define (grounded? f)
  (>= (foe-y f) SCREENH))

;; Proj -> Boolean
;; produce true if the Proj has touched the roof
(check-expect (roofed? P1) false)
(check-expect (roofed? P2) false)
(check-expect (roofed? P3) true)
(define (roofed? p)
  (<= (proj-y p) 0))

;; Foe -> Boolean
;; produce true if the foe has touched the sides
(check-expect (bounce-foe? F5) true)
(check-expect (bounce-foe? F1) false)
(check-expect (bounce-foe? F6) true)
(check-expect (bounce-foe? F7) true)
(check-expect (bounce-foe? F2) false)
(define (bounce-foe? f)
  (or (>= (foe-x f) (- SCREENW (/ (image-width FOE) 2))) (<= (foe-x f) (+ 0 (/ (image-width FOE) 2)))))
      

;; Foe -> Foe
;; produce the next foe
(check-expect (update-foe F1)
              (make-foe (+ (foe-x F1) (foe-dx F1)) (+ (foe-y F1) FOE-VEL) (foe-dx F1)))
(check-expect (update-foe F2)
              (make-foe (+ (foe-x F2) (foe-dx F2)) (+ (foe-y F2) FOE-VEL) (foe-dx F2)))
(define (update-foe f)
  (if (bounce-foe? f)
      (make-foe (+ (foe-x f) (- (foe-dx f))) (+ (foe-y f) FOE-VEL) (- (foe-dx f)))
      (make-foe (+ (foe-x f) (foe-dx f)) (+ (foe-y f) FOE-VEL) (foe-dx f))))

;; ListOfFoe -> ListOfFoe
;; produce the next list of foes by updating them
;(check-expect (update-foes LOF1) LOF1x)
;(check-expect (update-foes LOF2) LOF2x)
(define (update-foes lof)
  (cond [(empty? lof) empty]
        [else
         (if (grounded? (first lof))
             (cons (make-foe (random SCREENW) 0 (cond [(<= 5 (random 10)) (- FOE-SWNG)]
                                                      [else FOE-SWNG]
                                                      ))
                   (update-foes (rest lof)))
             (cons (update-foe (first lof))
                   (update-foes (rest lof))))]))


;; Proj -> Proj
;; produce the next Proj
(check-expect (update-proj P1)
              (make-proj (proj-x P1) (- (proj-y P1) PROJ-VEL)))
(check-expect (update-proj P2)
              (make-proj (proj-x P2) (- (proj-y P2) PROJ-VEL)))
(define (update-proj p)
  (make-proj (proj-x p) (- (proj-y p) PROJ-VEL)))


;; ListOfProj -> ListOfProj
;; produce the next list of Proj by updating them
;(check-expect (update-projs LOP1 T1) LOP1x)
;(check-expect (update-projs LOP2 T2) LOP2x)
(define (update-projs lop t)
  (cond [(empty? lop) empty]
        [else
         (cons (update-proj (first lop))
               (update-projs (rest lop) t))]))
;; ListOfFoe -> ListOfPosn
;; Create a list of positions
(check-expect (posn-foes LOF1) empty)
(check-expect (posn-foes LOF2)
              (list (make-posn (foe-x F1) (foe-y F1))
                    (make-posn (foe-x F2) (foe-y F2))))
(define (posn-foes lof)
  (cond [(empty? lof) empty]
        [else
         (if (empty? (rest lof))
             (make-posn (foe-x (first lof)) (foe-y (first lof)))
             (list (make-posn (foe-x (first lof)) (foe-y (first lof)))
                   (posn-foes (rest lof))))]))


;; Foe -> Boolean
;; produce true if the foe has touched the sides
(check-expect (bounce-tank? T1) true)
(check-expect (bounce-tank? T2) false)
(define (bounce-tank? f)
  (or (>= (tank-x f) (- SCREENW (/ (image-width TANK) 2))) (<= (tank-x f) (+ 0 (/ (image-width TANK) 2)))))

;; Tank -> Tank
;; produce the next tank
(check-expect (update-tank T1) T1x)
(check-expect (update-tank T2) T2x)
(define (update-tank t)
  (make-tank (+ (tank-x t) (tank-dx t)) TANK-Y (tank-dx t)))


;; Tank -> Tank
;; produce the next tank
(check-expect (bounce-tank T3 TANK-VEL) T3x)
(check-expect (bounce-tank T4 (- TANK-VEL)) T4x)
(define (bounce-tank t v)
  (make-tank (tank-x t) TANK-Y v))

;; Proj Foe -> Boolean
(define (collided? p f)
  (and (>= (proj-x p) (- (foe-x f) (/ (image-width FOE) 2)))
       (<= (proj-x p) (+ (foe-x f) (/ (image-width FOE) 2)))
       (>= (proj-y p) (- (foe-y f) (/ (image-width FOE) 2)))
       (<= (proj-y p) (+ (foe-y f) (/ (image-width FOE) 2)))))

;; Foe ListOfProj -> Boolean
;; produce true if any of the projectiles collide with the foe.
(check-expect (any-proj-collided? (make-foe 10 10 -5) (list (make-proj 10 10) (make-proj 500 500))) true)
(check-expect (any-proj-collided? (make-foe 52 52 5) (list  (make-proj 500 500) (make-proj 600 600))) false)
(define (any-proj-collided? f lop)
  (cond [(empty? lop) false]
        [else
          (if (collided? (first lop) f)
              true
              (any-proj-collided? f (rest lop)))]))

;; ListOfFoe ListOfProj -> ListOfFoe
;; produce a new list of foes while filtering our foes that collided with any projectiles
(check-expect (filter-foes (list (make-foe 10 10 -5) (make-foe 70 70 5))
                           (list (make-proj 10 10) (make-proj 500 500)))
              (list (make-foe 70 70 5)))
(check-expect (filter-foes (list (make-foe 50 50 -5) (make-foe 100 100 5) (make-foe 400 400 2))
                           (list (make-proj 100 100) (make-proj 500 500)))
              (list (make-foe 50 50 -5) (make-foe 400 400 2)))
(define (filter-foes lof lop)
  (cond [(empty? lof) empty]
        [else
         (if (any-proj-collided? (first lof) lop)
             (filter-foes (rest lof) lop)
             (cons (first lof)
                   (filter-foes (rest lof) lop)))]))

;; ListOfFoe | ListOfProj -> ListOfPosns
;; produce a new list of positions from the given objects
(check-expect (make-posns LOP2) (list (make-posn (proj-x P1) (proj-y P1)) (make-posn (proj-x P2) (proj-y P2))))
(check-expect (make-posns LOF2) (list (make-posn (foe-x F1) (foe-y F1)) (make-posn (foe-x F2) (foe-y F2))))
(define (make-posns loo)
  (cond [(empty? loo) empty]
        [else
         (if (proj? (first loo))
             (cons (make-posn (proj-x (first loo)) (proj-y (first loo)))
                   (make-posns (rest loo)))
             (cons (make-posn (foe-x (first loo)) (foe-y (first loo)))
                   (make-posns (rest loo))))]))

;; Game -> Game
;; produce the next Game object
;(check-expect (tock G1) G1x)
;(check-expect (tock G2) G2x)
(define (tock g)
  (make-game (update-foes (filter-foes (game-foes g) (game-projs g)))
             (update-tank (game-tank g))
             (update-projs (game-projs g) (game-tank g))))


;; Game -> Image
;; render the current game state using the Game structure
(define (render g)
  (place-images
   (append (list TANK)
           (repeat-foes (game-foes g))
           (repeat-projs (game-projs g)))
   (append (list (posn-tank (game-tank g)))
           (make-posns (game-foes g))
           (make-posns (game-projs g)))
   MTS))


;; Game KeyEvent -> Game
;; produce a new game state with tank's position changed by TANK-VEL
(check-expect (control G1 "right") (make-game (game-foes G1) (make-tank (tank-x (game-tank G1)) (tank-y (game-tank G1)) (tank-dx (game-tank G1))) (game-projs G1)))
(check-expect (control G2 "left") (make-game (game-foes G2) (make-tank (tank-x (game-tank G2)) (tank-y (game-tank G2)) (- (tank-dx (game-tank G2)))) (game-projs G2))) 

(define (control g ke)
  (make-game (game-foes g)
             (cond [(key=? ke "left") (bounce-tank (game-tank g) (- TANK-VEL))]
                   [(key=? ke "right") (bounce-tank (game-tank g) TANK-VEL)]
                   [else
                    (game-tank g)]
                   )
             (cond [(key=? ke " ") (insert-proj (game-projs g) (game-tank g))]
                   [else
                    (game-projs g)])
             )
  )

;=================================

;; Game -> Game
;; start the world with Game

(define (main g)
  (big-bang g                   ; Game
    (on-tick   tock)     ; Game -> Game
    (to-draw   render)   ; Game -> Image
    (on-key   control)))    ; Game KeyEvent -> Game
