#lang racket
;;; relaxation.rkt
;;; DrRacket 6.10
;;; Projet "Visualisation de graphe"

(require "positioning.rkt" "vect2D.rkt" "graph.rkt")

(define (new-relaxator)
  ;; Definition des constantes c1, c2, c3 et
  (let ([relax-c1 2 ]
        [relax-c2 20]
        [relax-c3 400]
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

        ;; Relax envoie une nouvelle position qui est est égale a c4 * Ft
        ;; Où Ft est la force totale, c'est a dire, la somme des forces mecaniques
        ;; et force electrique
        ((relax)
         (for ((i (in-list (get-nodes (first args)))))
           (let  ([vect-t (make-vect 0 0)])
             (for ((j (in-list (get-nodes (first args)))))
               (when (not (equal? i j))
                 (set! vect-t (vect-sum vect-t (force-e (second args) i j)))
                 (when (set-member? (get-neighbors (first args) i) j)
                   (set! vect-t (vect-sum vect-t (force-m (second args) i j))))))
             (positioning-move-node! (second args) i (vect-scalar relax-c4 vect-t)))))

        (else (error "Unknown method: " methode))))
        
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
      
      
    ;;  __________████████_____██████
    ;;_________█░░░░░░░░██_██░░░░░░█
    ;;________█░░░░░░░░░░░█░░░░░░░░░█
    ;;_______█░░░░░░░███░░░█░░░░░░░░░█
    ;;_______█░░░░███░░░███░█░░░████░█
    ;;______█░░░██░░░░░░░░███░██░░░░██
    ;;_____█░░░░░░░░░░░░░░░░░█░░░░░░░░███
    ;;____█░░░░░░░░░░░░░██████░░░░░████░░█
    ;;____█░░░░░░░░░█████░░░████░░██░░██░░█
    ;;___██░░░░░░░███░░░░░░░░░░█░░░░░░░░███
    ;;__█░░░░░░░░░░░░░░█████████░░█████████
    ;;_█░░░░░░░░░░█████_████___████_█████___█
    ;;_█░░░░░░░░░░█______█_███__█_____███_█___█
    ;;█░░░░░░░░░░░░█___████_████____██_██████
    ;;░░░░░░░░░░░░░█████████░░░████████░░░█
    ;;░░░░░░░░░░░░░░░░█░░░░░█░░░░░░░░░░░░█
    ;;░░░░░░░░░░░░░░░░░░░░██░░░░█░░░░░░██
    ;;░░░░░░░░░░░░░░░░░░██░░░░░░░███████
    ;;░░░░░░░░░░░░░░░░██░░░░░░░░░░█░░░░░█
    ;;░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█
    ;;░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█
    ;;░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█
    ;;░░░░░░░░░░░█████████░░░░░░░░░░░░░░██
    ;;░░░░░░░░░░█▒▒▒▒▒▒▒▒███████████████▒▒█
    ;;░░░░░░░░░█▒▒███████▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    ;;░░░░░░░░░█▒▒▒▒▒▒▒▒▒█████████████████
    ;;░░░░░░░░░░████████▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    ;;░░░░░░░░░░░░░░░░░░██████████████████
    ;;░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█
    ;;██░░░░░░░░░░░░░░░░░░░░░░░░░░░██
    ;;▓██░░░░░░░░░░░░░░░░░░░░░░░░██
    ;;▓▓▓███░░░░░░░░░░░░░░░░░░░░█
    ;;▓▓▓▓▓▓███░░░░░░░░░░░░░░░██
    ;;▓▓▓▓▓▓▓▓▓███████████████▓▓█
    ;;▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██
    ;;▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓█
    ;;▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓█
        
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

;; Test object relaxator
(define my-relaxator (new-relaxator))