#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt")
(require racket/random)

;; Constantes
(define RED-PEN (make-object pen% "red" 8 'solid))
(define BLACK-PEN (make-object pen% "black" 1 'solid))
(define WIDTH 500)
(define HEIGHT 500)
(define g (grid-graph 6 3))
(define graph-x 0)
(define graph-y 0)
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

;; Definitions graphiques
(define FRAME (new frame% (label "Visualisation graphique")))

(define VPANEL (new vertical-panel%
                    (parent FRAME)
                    (alignment '(left center))))

(define HPANEL (new horizontal-panel%
                    (parent VPANEL)
                    (alignment '(left top))))


(define CANVAS (new canvas%
                    (parent VPANEL)
                    (min-width WIDTH)
                    (min-height HEIGHT)
                    (paint-callback (lambda (obj dc)
                                      (send BITMAP-DC clear)
                                      (send BITMAP-DC set-smoothing 'smoothed)
                                      (dessiner-liens g e)
                                      (dessiner-sommets g e)
                                      (send dc draw-bitmap BITMAP graph-x graph-y 'solid)
                                      (r 'relax g e)))))

(define pause (read-bitmap "PAUSE.png"))
(define play (read-bitmap "PLAY.png"))
(define accelerer (read-bitmap "ACCELERER.png"))



;; Button pause/start
(define PAUSE
  (new button%
       (label play)
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (if (equal? animation #t)
              (begin
                (send TIMER stop)
                (set! animation #f)
                (send PAUSE set-label play))
              (begin
                (send TIMER start 20)
                (set! animation #t)
                (send PAUSE set-label pause)))))))


(define REMOVESommet
  (let ([SommetAlea 0])
  (new button%
       (label "RemoveSommet")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (when (not (equal? (hash-count g) 0))
           (begin (set! SommetAlea (random-ref (hash-keys e)))
          (rm-node! g SommetAlea)
          (hash-remove! e SommetAlea)
          (send CANVAS on-paint))))))))
          
  

;enlever le bouton quand length=1

(send FRAME show #t)



(define AddArrete
  (let ([Sommet empty]
        [SommetAlea empty]
        [IsFilter empty]
                                   
        [voisin 0])   ;Faire un filter pour voir si le sommet n'a pas déjà tous les sommets comme voisin
  (new button%
       (label "AddArrete")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (set! Sommet (hash-keys g))
          (set! IsFilter (filter (lambda (x) (not (equal? (- (hash-count g) 1) (length (set->list (hash-ref g x)))))) (hash-keys g)))
          (when (not (equal? IsFilter empty))
          (set! SommetAlea (random-ref IsFilter))
          (set! voisin (set->list (hash-ref g SommetAlea)))
            (for ((i (in-list voisin)))
              (set! Sommet (remove i Sommet)))
          (set! Sommet (remove SommetAlea Sommet))
          (add-edge! g SommetAlea (random-ref Sommet))
            (send CANVAS on-paint)))))))

(define AddSommet
  (new button%
       (label "AddSommet")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
  (let ([Sommet (hash-keys g)]
        [maximum 0])
    
  (set! Sommet (hash-keys g))
  (if (> (length Sommet) 0)
      (begin (set! maximum (apply max Sommet))
             (add-node! g (+ maximum 1))
             (hash-set! e (+ maximum 1) (make-vect (random WIDTH) (random HEIGHT))))
       (begin (add-node! g 0)
              (hash-set! e 0 (make-vect (random WIDTH) (random HEIGHT)))))
    (send CANVAS on-paint))))))
          
          


;random-ref?





(define REMOVEArr
  (let ([SommetAlea 0]
        [ArreteAlea 0]
        [IsFilter empty]
        )
  (new button%
       (label "removearr")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (set! IsFilter (filter (lambda (x) (not (equal? (mutable-set) (hash-ref g x)))) (hash-keys g)))
          (when (not (equal? IsFilter empty))
          ;tester si il y a au moins un sommet qui a un voisin
          (set! SommetAlea (random-ref  IsFilter))
         (set! ArreteAlea (random-ref (set->list (hash-ref g SommetAlea))))
          (rm-edge! g SommetAlea ArreteAlea)
            (send CANVAS on-paint)))))))

(define zoom
  (new button%
       (label "zoom")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send BITMAP-DC scale 1.5 1.5)
        ))))
          ;jpp
         
         

(define dezoom
  (new button%
       (label "dezoom")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send BITMAP-DC scale 0.5 0.5)))))

(define ralenti
  (new button%
       (label "ralenti")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send TIMER start (inexact->exact (round (* (send TIMER interval) 1.5))))))))

(define accèlerer
  (new button%
       (label accelerer)
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt) 
          (send TIMER start (inexact->exact (+ (round (* (send TIMER interval) 0.5)) 1)))))))

(define modifier
  (new text-field%
       (label "modifier")
       (parent FRAME)
       (init-value "0")))
       

(define modifierbutton
  (new button%
       (label "Modifier")
       (parent FRAME)
       
       (style '(border))
       (callback
        (lambda (obj evt)
          (let ((n (string->number (send modifier get-value))))
                 (my-relaxator 'set-c1 n))))))




(define reinitialisation
  (new button%
       (label "reinitialisation")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (set! g graphtempo)))))