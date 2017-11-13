#lang racket
;;; relaxation.rkt
;;; DrRacket 6.10
;;; Projet "Visualisation de graphe"
(require "positioning.rkt" "vect2D.rkt")

(define (new-relaxator)
  (let ([relax-c1 2 ]
        [relax-c2 1]
        [relax-c3 1]
        [relax-c4 0.1])
    (define (this methode . args)
      (case methode

        ;; setters
        ((set-c1)(set! relax-c1 (car args)))
        ((set-c2)(set! relax-c2 (car args)))
        ((set-c3)(set! relax-c3 (car args)))
        ((set-c4)(set! relax-c4 (car args)))

        ;; getters
        ((get-c1) relax-c1)
        ((get-c2) relax-c2)
        ((get-c3) relax-c3)
        ((get-c4) relax-c4)
        
        ;; Force mecanique
        ((force-m)
         (when (and (hash-has-key? (first args) (second args))(hash-has-key? (first args)(third args)))
            (let ([x (hash-ref (first args) (second args))]
              [y (hash-ref (first args) (third args))])
          (define u (vect-unit (vect-mult x y)))
          (define d (sqr (vect-norm (vect-mult x y))))
          (define k (* -1 (/ relax-c3 d)))
          (define res (vect-scalar k u))
          res)))
        
        ;; Force electrique
        ((force-e)
         (when (and (hash-has-key? (first args) (second args))(hash-has-key? (first args)(third args)))
            (let ([x (hash-ref (first args) (second args))]
              [y (hash-ref (first args) (third args))])
          (define u (vect-unit (vect-mult x y)))
          (define d (sqr (vect-norm (vect-mult x y))))
          (define k (* -1(/ relax-c3 d)))
          (define res (vect-scalar k u))
          res)))

        (else (error "Unknown method: " methode))))this))
        
        
           
           
                    

        
        
        
  