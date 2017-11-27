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
(define HPANEL
  (new horizontal-panel%
  (parent FRAME)))

(define HHPANEL
  (new horizontal-panel%
       (parent FRAME)))

(define horizontal-canvas
  (new horizontal-panel%
       (parent FRAME)))

(define VPANEL
  (new vertical-panel%
       (parent HPANEL)))



;; Button pause/start

(define hpanel2
  (new horizontal-panel%
       [parent VPANEL]))
(define PAUSE
  (new button%
       (label "Start")
       (parent hpanel2)
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

(define REMOVE
  (let ([listekeys (hash-keys e)]
        [aleatoire 1])
  (new button%
       (label "Enlever sommet")
       (parent hpanel2)
       ;(min-width 200)
       (style '(border))
       (callback
        (lambda (obj evt)
          (when (not (empty? listekeys))
          (set! aleatoire (list-ref listekeys (random (length listekeys))))
          (set! listekeys (remove aleatoire listekeys))
          (rm-node! g aleatoire)
          (hash-remove! e aleatoire)))))))

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
                                      (r 'relax g e)))))
(define OPTIONS
  (new dialog%
       (label "options")
       (width 200)
       (height 200)))

(define vpanel2
  (new vertical-panel%
       (parent HPANEL)))

(define slider-c1
  (new slider%
       (parent OPTIONS)
       (label "C1")
       (style '(vertical))
       (min-value 1)
       (max-value 60)
       (init-value (r 'get-c1))))

(define slider-c2
  (new slider%
       (parent OPTIONS)
       (label "C2")
       (style '(vertical))
       (min-value 5)
       (max-value 60)
       (init-value (r 'get-c2))))
       
(define inshallah
  (new button%
       (label "test")
       (parent OPTIONS)
       (style '(border))
       (callback
        (lambda (obj evt)
          (r 'set-c1 (send slider-c1 get-value))
          (r 'set-c2 (send slider-c2 get-value))
          (r 'set-c3 (sqr (send slider-c2 get-value)))
          (send OPTIONS show #f)))))
(send FRAME show #t)



(define opt-button
  (new button%
       (label "Options")
       (parent hpanel2)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send OPTIONS show #t)))))




       
       





          


