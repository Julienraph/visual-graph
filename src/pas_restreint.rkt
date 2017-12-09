#lang racket/gui
(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt")

(define RED-PEN (make-object pen% "red" 8 'solid))
(define BLACK-PEN (make-object pen% "black" 1 'solid))
(define WIDTH 5)
(define HEIGHT 5)
(define g (grid-graph 3 3))
(define e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
(define r (new-relaxator))

;; Double buffer
(define BITMAP (make-object bitmap% WIDTH HEIGHT))
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINITIONS PRATIQUES ;;;;;;;;;;;

;; Fonction qui dessine les sommets
(define (dessiner-sommets g e)
  (for ([(k v) (in-hash e)])
    
    (send BITMAP-DC set-pen RED-PEN)
    (send BITMAP-DC draw-point (coord-x v) (coord-y v))))

;; Fonction qui dessine les liens entre les sommets
(define (dessiner-liens g e)
  (for ([(k v) (in-hash e)])
    (when (hash-has-key? g k)
      (for ((i (in-set (get-neighbors g k))))
        (when (hash-has-key? g k)
          (send BITMAP-DC set-pen BLACK-PEN)
          (send BITMAP-DC draw-line (coord-x (hash-ref e i)) (coord-y (hash-ref e i))
                (coord-x v) (coord-y v)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINITIONS GRAPHIQUES ;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; FRAMES ;;;;;;;;;;;;;;;;;;;;;

(define FRAME (new frame% (label "Visualisation graphique")))

(define HPANEL
  (new horizontal-panel%
       (parent FRAME)
       ))

(define VPANEL
  (new vertical-panel%
       (parent HPANEL)
       ))

;; Boucle 
(for ([i 20])
  (r 'relax g e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; CANVAS ;;;;;;;;;;;;;;;;;;;;;

;; Dessin du graphique
(define CANVAS (new canvas%
                    (parent VPANEL)
                    (min-width WIDTH)
                    (min-height HEIGHT)
                    (paint-callback (lambda (obj dc)
                                      (send BITMAP-DC clear)
                                      (send BITMAP-DC set-smoothing 'smoothed)
                                      (dessiner-liens g e)
                                      (dessiner-sommets g e)
                                      (send dc draw-bitmap BITMAP 0 0 'solid)
                                      ))))


