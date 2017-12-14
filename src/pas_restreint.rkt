#lang racket/gui
(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt")

(define RED-PEN (make-object pen% "red" 0.08 'solid))
(define BLACK-PEN (make-object pen% "black" 0.08 'solid))
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


(r 'set-c3 1)
(r 'set-c1 2.8)
(r 'set-c2 0)
(r 'set-c4 0.001)
;; Boucle 
(for ([i 10000])
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

(define (distance g e)
  (let ([d 0]
        [res empty])
  (for ([(k v) (in-hash e)])
    (for ([i (in-list (set->list (get-neighbors g k)))])
    (set! d (sqr (vect-norm (vect-sum (vect-scalar -1 (hash-ref e k)) (hash-ref e i)))))
     (set! res (append res (list d)))))res))


(*(/(-(apply max (distance g e))  (apply min (distance g e)))(apply min (distance g e))) 100)

;(send FRAME show #t)


