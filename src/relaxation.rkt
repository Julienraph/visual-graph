#lang racket
;;; relaxation.rkt
;;; DrRacket 6.10
;;; Projet "Visualisation de graphe"
(require "positioning.rkt" "vect2D.rkt")
;; c2 = d
;; c3 = d^2
(define relaxator%
  (class object%

    ;; d = 0
    (define-values (relax-c1 relax-c2 relax-c3 relax-c4)(values 2 0 0 0.1))

    ;; setters
    (define/public (set-c1 n)
      (define relax-c1 n))
    (define/public (set-c2 n)
      (define relax-c2 n))
    (define/public (set-c3 n)
      (define relax-c3 n))
    (define/public (set-c4 n)
      (define relax-c4 n))

    ;; getters
    (define/public (get-c1)
      relax-c1)
    (define/public (get-c3)
      relax-c2)
    (define/public (get-c3)
      relax-c3)
    (define/public (get-c4)
      relax-c4)

    (super-new))
    
    
  
  


