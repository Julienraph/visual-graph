#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt")

(define RED-PEN (make-object pen% "red" 10 'solid))
(define BLACK-PEN (make-object pen% "black" 1 'solid))
(define TIMER
  (new timer% (notify-callback (lambda () (send CANVAS on-paint)))))

(define FRAME (new frame% (label "test-canvas")))
(define CANVAS (new canvas%
                    (parent FRAME)
                    
                    (min-width 500)
                    (min-height 500)

                    ; Dessin
                    (paint-callback
                     (lambda (obj evt)
                     (let ([dc (send obj get-dc)])
                       (send dc clear)
                       (for ([(k v) (in-hash e)])
                         (for ((i (in-set (get-neighbors g k))))
                           
                           ; Associations de noeud
                           (send dc set-pen BLACK-PEN)
                           (send dc draw-line (coord-x (hash-ref e i)) (coord-y (hash-ref e i))
                           (coord-x v) (coord-y v))
                           
                           ; Noeuds
                           (send dc set-pen RED-PEN)
                           (send dc draw-point (coord-x v) (coord-y v))))
                       (r 'relax g e))))))

                           


; Creation d'un graphe avec un sommet A lie à (b c d e)
(define g (grid-graph 6 3))


;; Positions random a chaque noeud        
(define e (random-positioning-of-node-list 500 500 (get-nodes g)))
(define r (new-relaxator))

                       
(send FRAME show #t)
(send TIMER start 20)