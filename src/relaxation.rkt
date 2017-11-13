#lang racket
;;; relaxation.rkt
;;; DrRacket 6.10
;;; Projet "Visualisation de graphe"
(require "positioning.rkt" "vect2D.rkt")

;; c2 = d
;; c3 = d^2
(define relaxator%
  (class object%

    ;; d = 1
    (define-values (relax-c1 relax-c2 relax-c3 relax-c4)(values 2 1 1 0.1))

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

    (define/public (force-m positioning node-id1 node-id2)
      (when (and (hash-has-key? positioning node-id1) (hash-has-key? positioning node-id2))
        (let ([x (hash-ref node-id1)]
              [y (hash-ref node-id2)])
          (define u (vect-unit (vect-mult x y)))
          (define d (vect-norm (vect-mult x y)))
          (define k (* relax-c1 (log (/ d relax-c2))))
          (define res (vect-scalar k u))
          res)))

    (define/public (force-e positioning node-id1 node-id2)
      (when (and (hash-has-key? positioning node-id1) (hash-has-key? positioning node-id2))
        (let ([x (hash-ref positioning node-id1)]
              [y (hash-ref positioning node-id2)])
          (define u (vect-unit (vect-mult x y)))
          (define d (sqr (vect-norm (vect-mult x y))))
          (define k (* -1(/ relax-c3 d)))
          (define res (vect-scalar k u))
          res)))
   (super-new)