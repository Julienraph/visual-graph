#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt" 2htdp/image)

(define RED-PEN (make-object pen% "red" 10 'solid))
(define BLACK-PEN (make-object pen% "black" 1 'solid))
(define TIMER
  (new timer% (notify-callback (lambda () (send CANVAS on-paint)))))
(define pepe(bitmap "pepe.png"))

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
                           (send dc draw-text "Do you have time to talk" 150 5)
                           (send dc draw-text "about our lord and savior" 150 25)
                          (send dc draw-text "jesus christ ?" 150 45)
                           ; Associations de noeud
                           (send dc set-pen BLACK-PEN)
                           (send dc draw-line (coord-x (hash-ref e i)) (coord-y (hash-ref e i))
                           (coord-x v) (coord-y v))
                           
                           ; Noeuds
                           (send dc set-pen RED-PEN)
                           (send dc draw-point (coord-x v) (coord-y v))
                           (send dc draw-text (number->string k) (coord-x v) (coord-y v))))
                       (r 'relax g e))))))
                           


; Creation d'un graphe avec un sommet A lie à (b c d e)
(define g (grid-graph 6 3))

;(define tempograph
 ; (append e empty))


;; Positions random a chaque noeud        
(define e (random-positioning-of-node-list 500 500 (get-nodes g)))
(define r (new-relaxator))

                       
(send FRAME show #t)
(send TIMER start 1)
(send TIMER interval)

(define animation
  #t)

(define PAUSE
  (new button%
       (label "PAUSE")
       (parent FRAME)
       (style '(border))
       (callback
          (lambda (obj evt)
            (if (equal? animation #t)
                    (begin (send TIMER stop)
                    (set! animation #f))
                (begin (send TIMER start 20)
                       (set! animation #t)))))))

;(define REMBOBINER
 ; (new button%
  ;     (label "REMBOBINER")
   ;    (parent FRAME)
    ;   (style '(border))
     ;  (callback
      ;  (lambda (obj evt)
       ;   (for ([in-list tempograph])
        ;    (set! tempograph (cdr tempograph)

(define (fonction x)
      (* (+ x 4) 4))

    (define (inverse x)
      (/ 1 (fonction x)))


            


            