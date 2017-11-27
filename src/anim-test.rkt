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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Definitions graphiques ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define FRAME (new frame% (label "Visualisation graphique")))

(define HPANEL
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
                                      (r 'relax g e)))))

;; Fenetre choix: Nouveau graphique ou importer
(define START-DIALOG
  (new dialog%
       (label "Visualisation graphique")
       (width 300)
       (height 100)))

;; Panel pour organiser les buttons
(define START-DIALOG-PANEL (new horizontal-panel%
                                [parent START-DIALOG]
                                [alignment '(center center)]))

;; Button de creation de graphe
(define new-graph-button
  (new button%
       [parent START-DIALOG-PANEL]
       [label "Nouveau graphique"]
       (callback
        (lambda (obj evt)
          (send FRAME show #t)
          (send START-DIALOG show #f)))))


;; Button d'importation de graphique
(define import-graph-button
  (new button%
       [parent START-DIALOG-PANEL]
       [label "Importer graphique"]
       [callback
        (lambda (obj evt)
          (define imported-file
            (get-file "Importer graphique" #f #f #f #f null '(("Any" "*.gv"))))
          (printf "~a" imported-file)
          (send START-DIALOG show #f))]))


(send START-DIALOG show #t)