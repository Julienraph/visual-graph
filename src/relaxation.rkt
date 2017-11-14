#lang racket
;;; relaxation.rkt
;;; DrRacket 6.10
;;; Projet "Visualisation de graphe"
(require "positioning.rkt" "vect2D.rkt" "graph.rkt")

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

        ((test-tm)(force-tm (first args) (second args)))
        ((test-m) (force-m (first args) (second args) (third args)))
        ((test-e) (force-e (first args) (second args) (third args)))
        ((test-te) (force-te (first args) (second args)))
        ((test-t) (force-t (first args) (second args)))
        
        (else (error "Unknown method: " methode)))) ; < -- fin de la function this
        
    ;; Force mecanique
    (define (force-m positioning node-id1 node-id2)
      (when (and (hash-has-key? positioning node-id1) (hash-has-key? positioning node-id2))
        (let ([vect1 (hash-ref positioning node-id1)]
              [vect2 (hash-ref positioning node-id2)])
          (define u (vect-unit (vect-sum (vect-scalar -1 vect1) vect2)))
          (define d (sqr (vect-norm (vect-sum (vect-scalar -1 vect1) vect2))))
          (define k (* relax-c1 (log(/ d relax-c3 ))))
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

    (define (force-tm positioning graph)
      (let ([vect-m (make-vect 0 0)])
      (for ([(k v)(in-hash graph)])
        (for ([i (in-set (get-neighbors graph k))])
          (printf "~a" vect-m)
          (set! vect-m (vect-sum vect-m (force-m positioning k i)))
          (printf "~a" vect-m)))
        vect-m))

    (define (force-te positioning graph)
      (let ([vect-e (make-vect 0 0)])
      (for ([(k v)(in-hash graph)])
        (for ([i (in-list (remove k (get-nodes graph)))])
          (printf "~a" vect-e)
          (set! vect-e (vect-sum vect-e (force-e positioning k i)))))
        vect-e))

    (define (force-t positioning graph)
      (let ([vect-t (make-vect 0 0)])
      (for ([(k v)(in-hash graph)])
        (for ([i (in-set (get-neighbors graph k))])
          (set! vect-t (vect-sum vect-t (force-m positioning k i)))
          (for ([j (in-list (remove k (get-nodes graph)))])
          (printf "~a" vect-t)
          (set! vect-t (vect-sum vect-t (force-e positioning k j))))))
        vect-t))
    
          
        

      
    this))

;; VARIABLES DE TEST

(define g (empty-graph))
(add-edge! g 'a 'b)

; Fonction de placement
(define e (positioning))

; Vecteur test 
(define t1 (make-vect 3 2))
(define t2 (make-vect 4 1))

;; Test de placement
(apply-positioning e 'a t1)
(apply-positioning e 'b t2)

(define my-relaxator (new-relaxator))





;(define next-pos (my-relaxator 'test e g))
       
           
           
                    

        
        
        
    