#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt")

(define FRAME (new frame% (label "test-canvas")))
(define RED-PEN (make-object pen% "red" 3 'solid))
(define PINK-PEN (make-object pen% "pink" 2 'solid))
(define CANVAS (new canvas%
                    (parent FRAME)
                    (min-width 50)
                    (min-height 50)
                    (paint-callback
                     (lambda (obj evt)
                     (let ([dc (send obj get-dc)])
                       (send dc clear)
                       (send dc set-pen RED-PEN)
                       (send dc set-brush PINK-PEN)
                       (send dc draw-point (coord-x (hash-ref e 'a)) (coord-y (hash-ref e 'a)))
                       (send dc draw-point (coord-x (hash-ref e 'b)) (coord-y (hash-ref e 'b)))
                       (send dc draw-line (coord-x (hash-ref e 'a)) (coord-y (hash-ref e 'a))
                             (coord-x (hash-ref e 'b)) (coord-y (hash-ref e 'b))))))))
(define BUTTON
  (new button% (parent FRAME)(label "Go!")
       (callback (lambda (obj evt) (send CANVAS on-paint)))))
                           


; Fonction de placement
(define e (positioning))

; Vecteur test 1
(define t1 (make-vect 10 21))
(define t2 (make-vect 20 11))

;; Test de placement
(apply-positioning e 'a t1)
(apply-positioning e 'b t2)



(send FRAME show #t)