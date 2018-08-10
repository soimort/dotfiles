;;;; Ryan Burnside
;;;; 2014 LOGO like implementation for Gimp
;;;; If it blows up your computer or kills your cat don't come whining to me.
;;;; No honestly, I mean if your computer reboots from a fine OS into Windows
;;;; it isn't my fault.

;; Turtle is just an associative list but we write some nice methods
;; To give the illusion of a modern class

;*** TODO
;; Add a with-state macro
;; Possibly use stroked paths instead of lines, let user stroke path
;; Add a macro to copy a turtle
;; Possibly add a flood fill command like some other LOGO systems

;; Consult UCB Logo implimentation for more ideas
;***


;; NOTES
;; The turtle will not consider the current context when drawing Script-Fu error
;; The functions to get current image and layer assume most recently created
;; Utility functions


(macro (help macro-name)
  "Returns the DocString of a procedure or macro
   (help macro-name)"
  (let ((expr (gensym)))
    `(let ((,expr (get-closure-code ,(cadr macro-name))))
       (and ,expr
            (cond
              ((string? (caddr ,expr))
                (caddr ,expr))
              ((and (pair? (caddr ,expr)) (eq? 'apply (caaddr ,expr)))
                (caddr (cadr (caddr ,expr))))
              #f )))))

(define-macro (repeat num-times . body)
  "Example (repeat 6 (print 'hello))"
  `(let ((counter 0))
     (while (< counter ,num-times)
	    ,@body
	    (set! counter (+ counter 1)))))

(define-macro (do-times variable num-times . body)
  "Example (do-times i 6 (print i))"
  `(let ((,variable 0))
     (while (< ,variable ,num-times)
	    ,@body
	    (set! ,variable (+ ,variable 1)))))

(define-macro (with-undo-group . body)
  `(apply 
    (lambda()
    (gimp-image-undo-group-start (get-current-image))
    ,@body
    (gimp-image-undo-group-end (get-current-image)))))

(define (deg-to-rad degrees)
  "Turns degrees into radians"
  (* degrees 0.0174532925))

(define (rad-to-deg radians)
  "Turns radians into degrees"
  (* radians 57.2957795))

(define (angle-wrap angle)
  "Forces angles to be [0 360)
   (angle-wrap angle)"
  (let* ((num-times (truncate (/ angle 360.0)))
	 (whole-part (* num-times 360.0))
	 (reduced (- angle whole-part)))
    (if (< reduced 0.0)
	(+ 360.0 reduced)
	reduced)))

(define (direction x y x2 y2)
  "Get direction between points
   (direction x y x2 y2)"
  ;Note, the y is inverted as LOGO considers "up" 90 degrees
  (angle-wrap (rad-to-deg (atan (- y y2) (- x2 x)))))

(define (distance x y x2 y2)
  "Preform a basic distance calculation
   (distance x y x2 y2)"
  (let ((a (- x2 x))
	(b (- y2 y)))
    (sqrt (+ (pow a 2) (pow b 2)))))

(define (get-current-image)
  "Return the image associated with the drawing"
  (car (gimp-image-list)))

(define (get-current-layer)
  "Return the current layer on focused image"
  (car (gimp-image-get-active-layer (vector-ref (cadr (gimp-image-list)) 0))))

(define (draw-line x y x2 y2)
  "Generic function to draw line in current brush
   (draw-line x y x2 y2)"
  (let ((layer (get-current-layer))
	(points (vector x y x2 y2)))
    (gimp-paintbrush-default layer 4 points)))

(define (Turtle-draw-line x y x2 y2 Turtle)
  "Draw a line in the current brush and Turtle's color"
  (gimp-context-set-foreground (get-attribute 'color Turtle))
  (draw-line x y x2 y2))

(define (make-Turtle x y direction)
  "Make a fresh turtle with x y and direction
   (make-Turtle x y direction)"
  `((x ,x)
    (y ,y)
    (direction ,direction)
    (color "#000000") ; Hex color default is black
    (drawing #t)
    (draw-command #f)))

;; Some nice wrappers which will be ... wrapped further
(define (get-attribute attrib list)
  "May be used to manually get attribute (should be wrapped for user)"
  (cadr (assq attrib list)))

(define (set-attribute! attrib value list)
  "May be used to manually set attribute (should be wrapped for user)"
  (set-car! (cdr (assq attrib list)) value))

;;; Turtle specific functions

;; Relative movement commands
(define (color hex-string Turtle)
  "Set the color of the turtle ex #FFFFFF
   (color hex-string Turtle)"
  (set-attribute! 'color hex-string Turtle))

(define (rt direction Turtle)
  "Rotate right current direction + direction
   (rt direction Turtle)"
  (set-attribute! 'direction
		  (angle-wrap (- (get-attribute 'direction Turtle) direction))
		  Turtle))

(define (lt direction Turtle)
  "Rotate left current direction + direction
   (lt direction Turtle)"
  (set-attribute! 'direction
		  (angle-wrap (+ (get-attribute 'direction Turtle) direction))
		  Turtle))

(define (fd steps Turtle)
  "Move steps in direction currently facing
   (fd steps Turtle)"
  (let* ((direction (deg-to-rad (get-attribute 'direction Turtle)))
	 (last-x (get-attribute 'x Turtle))
	 (last-y (get-attribute 'y Turtle))
	 (new-x (+ last-x (* (cos direction) steps)))
	 (new-y (- last-y (* (sin direction) steps))))
    (set-attribute! 'x new-x Turtle)
    (set-attribute! 'y new-y Turtle)
    (if (get-attribute 'drawing Turtle)
	(Turtle-draw-line last-x last-y new-x new-y Turtle))))

(define (bk steps Turtle)
  "Move steps backwards while keeping currently facing direction
  (bk steps Turtle)"
  (let* ((direction (deg-to-rad (+ 180 (get-attribute 'direction Turtle))))
	 (last-x (get-attribute 'x Turtle))
	 (last-y (get-attribute 'y Turtle))
	 (new-x (+ last-x (* (cos direction) steps)))
	 (new-y (- last-y (* (sin direction) steps))))
    (set-attribute! 'x new-x Turtle)
    (set-attribute! 'y new-y Turtle)
    (if (get-attribute 'drawing Turtle)
	(Turtle-draw-line last-x last-y new-x new-y Turtle))))

(define (pu Turtle)
  "Pen up, disable drawing 
   (pu Turtle)"
  (set-attribute! 'drawing #f Turtle))

(define (pd Turtle)
  "Pen down, enable drawing
   (pd Turtle)"
  (set-attribute! 'drawing #t Turtle))

(define (cs)
  "Clears all data on current image
   (cs)"
  (with-undo-group
   (gimp-selection-all (get-current-image))
   (gimp-edit-clear (get-current-layer))
   (gimp-selection-none (get-current-image))))

(define (show Turtle)
  "This command shoves a turtle shape onto the canvas, undo to remove it
   (show Turtle)"
  (print "Press undo to remove this turtle shape, it is drawn to canvas")
  ; Impliment some special turtle displaying layer later maybe...
  (with-undo-group
   (let ((pen-state (get-attribute 'drawing Turtle))
	 (angle-state (get-attribute 'direction Turtle))
	 (x (get-attribute 'x Turtle))
	 (y (get-attribute 'y Turtle))
	 (scale 24.0))
     (gimp-image-undo-group-start (get-current-image))
     (pu Turtle)
     (fd (* scale .625) Turtle)
     (pd Turtle)
     (rt (+ 180.0 26.5651) Turtle)
     (fd (* scale 1.118) Turtle)
     (lt (- 296.565 180.0) Turtle)
     (fd scale Turtle)
     (lt (- 296.565 180.0) Turtle)
     (fd (* scale 1.118) Turtle)
     (gimp-image-undo-group-end (get-current-image))
     (set-attribute! 'x x Turtle)
     (set-attribute! 'y y Turtle)
     (set-attribute! 'drawing pen-state Turtle)
     (set-attribute! 'direction angle-state Turtle))))

(define (fd-to x y Turtle)
  "Move to a position, drawing if pen is down
   (mv-to x y Turtle)"
  (let* ((turt-x (get-attribute 'x Turtle))
	 (turt-y (get-attribute 'y Turtle))
	 (d (direction turt-x turt-y x y))
	 (l (distance turt-x turt-y x y)))
    (lk d Turtle)
    (fd l Turtle)))

;; Absolute movement commands
(define (mv x y Turtle)
  "jump to position do not draw
   (mv x y Turtle)"
  (set-attribute! 'x x Turtle)
  (set-attribute! 'y y Turtle))

(define (lk angle Turtle)
  "Look and face an angle
   (lk angle Turtle)"
  (set-attribute! 'direction (angle-wrap angle) Turtle))

(define (home Turtle)
  "Move the Turtle to the center of the image and face up
   (home Turtle)"
  (mv (/ (car (gimp-image-width (get-current-image))) 2.0)
      (/ (car (gimp-image-height (get-current-image))) 2.0)
      Turtle)
  (lk 90 Turtle))

;; High level function example
(define (draw-star side-length Turtle)
   "Draw a 5 point star in current direction
   (draw-star side-length Turtle)"
   (with-undo-group
    (repeat 5
	    (fd side-length Turtle)
	    (rt 144 Turtle))))

(define (draw-rectangle height width Turtle)
  "Draw a rectangle in current direction
   (draw-rectangle height width Turtle)"
  (with-undo-group
   (fd width Turtle) (rt 90 Turtle)
   (fd height Turtle) (rt 90 Turtle)
   (fd width Turtle) (rt 90 Turtle)
   (fd height Turtle) (rt 90 Turtle)))

(define (draw-n-gon num-sides side-length Turtle)
   "Draw an n-gon of num-sides and side-length from turtle's facing position
   (draw-n-gon num-sides side-length Turtle)"
   (with-undo-group
    (let ((angle (/ 360.0 num-sides)))
      (repeat num-sides (fd side-length Turtle) (rt angle Turtle)))))

(define (draw-arrow-head Turtle)
  "Put a small arrow head in facing direction
   (draw-arrow-head Turtle)"
  (with-undo-group
   (lt 170 Turtle)
   (fd 20 Turtle)
   (bk 20 Turtle)
   (rt (* 2.0 170) Turtle)
   (fd 20 Turtle)
   (bk 20 Turtle)
   (lt 170 Turtle)))

(define (draw-arrow-to x y Turtle)
  "Make turtle draw to a point and put an arrow head on end
   (draw-arrow-to x y Turtle)"
  (with-undo-group
   (fd-to x y Turtle)
   (draw-arrow-head Turtle)))
