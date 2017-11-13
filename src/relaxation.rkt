#lang racket
;;; relaxation.rkt
;;; DrRacket 6.10
;;; Projet "Visualisation de graphe"
(require "positioning.rkt" "vect2D.rkt")

(define (new-relaxator)
  (let ([relax-c1 2 ]
        [relax-c2 20]
        [relax-c3 400]
        [relax-c4 0.1])
    (define (this methode . args)
      (case methode

        ;; setters
        (('set-c1)(set! relax-c1 (car args)))
        (('set-c2)(set! relax-c2 (car args)))
        (('set-c3)(set! relax-c3 (car args)))
        (('set-c4)(set! relax-c4 (car args)))

        ;; getters
        (('get-c1) relax-c1)
        (('get-c2) relax-c2)
        (('get-c3) relax-c3)
        (('get-c4) relax-c4)
        (else (error "Unknown method: " methode)))) ; < -- fin de la function this
        
    ;; Force mecanique
    (define (force-m positioning node-id1 node-id2)
      (when (and (hash-has-key? positioning node-id1) (hash-has-key? positioning node-id2))
        (let ([vect1 (hash-ref node-id1)]
              [vect2 (hash-ref node-id2)])
          (define u (vect-unit (vect-sum (vect-scalar -1 vect1) vect2)))
          (define d (sqr (vect-norm (vect-sum (vect-scalar -1 vect1) vect2))))
          (define k (* -1 (/ relax-c3 d)))
          (define res (vect-scalar k u))
          res)))
        
    ;; Force electrique
     (define (force-e positioning node-id1 node-id2)
      (when (and (hash-has-key? positioning node-id1) (hash-has-key? positioning node-id2))
        (let ([vect1 (hash-ref positioning node-id1)]
              [vect2 (hash-ref positioning node-id2)])
          (define u (vect-unit (vect-sum (vect-scalar -1 vect1) vect2)))
         (define d (sqr (vect-norm (vect-sum (vect-scalar -1 vect1) vect2))))
         (define k (* -1(/ relax-c3 d)))
         (define res (vect-scalar k u))
         res)))
    this))

        
        
        
           
           
                    

        
        
        
    