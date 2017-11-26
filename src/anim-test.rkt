#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt")

;; Constantes
(define RED-PEN (make-object pen% "red" 8 'solid))
(define BLACK-PEN (make-object pen% "black" 1 'solid))
(define WIDTH 500)
(define HEIGHT 500)
(define g (grid-graph 6 3))
(define e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
(define r (new-relaxator))
(define animation #f)

;; Double buffer
(define BITMAP (make-object bitmap% WIDTH HEIGHT))
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP)))

;; Horloge animation
(define TIMER (new timer% (notify-callback (lambda () (send CANVAS on-paint)))))

;; Fonction qui dessine les sommets
(define (dessiner-sommets g e)
  (for ([(k v) (in-hash e)])
    (when (hash-has-key? g k)
      (send BITMAP-DC set-pen RED-PEN)
      (send BITMAP-DC draw-point (coord-x v) (coord-y v)))))

;; Fonction qui dessine les liens entre les sommets
(define (dessiner-liens g e)
  (for ([(k v) (in-hash e)])
    (for ((i (in-set (get-neighbors g k))))
      (when (hash-has-key? g k)
        (send BITMAP-DC set-pen BLACK-PEN)
        (send BITMAP-DC draw-line (coord-x (hash-ref e i)) (coord-y (hash-ref e i))
              (coord-x v) (coord-y v))))))

;; Definitions graphiques
(define FRAME (new frame% (label "Visualisation graphique")))
(define CANVAS (new canvas%
                    (parent FRAME)
                    (min-width WIDTH)
                    (min-height HEIGHT)
                    (paint-callback (lambda (obj dc)
                                      (send BITMAP-DC clear)
                                      (send BITMAP-DC set-smoothing 'smoothed)
                                      (dessiner-liens g e)
                                      (dessiner-sommets g e)
                                      (send dc draw-bitmap BITMAP 0 0 'solid)
                                      (r 'relax g e)))))

;; Button pause/start
(define PAUSE
  (new button%
       (label "Start")
       (parent FRAME)
       (style '(border))
       (callback
        (lambda (obj evt)
          (if (equal? animation #t)
              (begin
                (send TIMER stop)
                (set! animation #f)
                (send PAUSE set-label "Start"))
              (begin
                (send TIMER start 20)
                (set! animation #t)
                (send PAUSE set-label "Pause")))))))

(send FRAME show #t)