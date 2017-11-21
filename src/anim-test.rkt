#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt" 2htdp/image)

;; Constantes
(define RED-PEN (make-object pen% "red" 8 'solid))
(define BLACK-PEN (make-object pen% "black" 1 'solid))
(define WIDTH 500)
(define HEIGHT 500)
(define g (grid-graph 6 3))
(define e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
(define r (new-relaxator))
(define animation #t)

;; Double buffer
(define BITMAP (make-object bitmap% WIDTH HEIGHT))
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP)))

;; Horloge animation
(define TIMER (new timer% (notify-callback (lambda () (send CANVAS on-paint)))))

;; Definitions graphiques
(define FRAME (new frame% (label "Visualisation graphique")))
(define CANVAS (new canvas%
                    (parent FRAME)
                    
                    (min-width WIDTH)
                    (min-height HEIGHT)

                    ; Dessin
                    (paint-callback
                     (lambda (obj evt)
                       (let ([dc (send obj get-dc)])
                         (send BITMAP-DC clear)
                         (send BITMAP-DC set-smoothing 'smoothed)
                         (for ([(k v) (in-hash e)])
                           (for ((i (in-set (get-neighbors g k))))
                             
                             ; Associations de noeud
                             (send BITMAP-DC set-pen BLACK-PEN)
                             (send BITMAP-DC draw-line (coord-x (hash-ref e i)) (coord-y (hash-ref e i))
                                   (coord-x v) (coord-y v))
                           
                             ; Noeuds
                             (send BITMAP-DC set-pen RED-PEN)
                             (send BITMAP-DC draw-point (coord-x v) (coord-y v))
                             (send dc draw-bitmap BITMAP 0 0 'solid)
                             ))
                         (r 'relax g e))))))

;; Button pause/start
(define PAUSE
  (new button%
       (label "Pause")
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
(send TIMER start 10)