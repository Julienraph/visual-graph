#lang racket
;;; vect2D.rkt
;;; DrRacket 6.10
;;; Module offrant des fonctionalités élémentaires
;;; sur les graphes et basé sur une representation
;;; par liste adjacence.
;;; Pour le projet "Visualisation de graphe"


(provide (all-defined-out))

; Creation d'un graphe vide
(define (empty-graph)
  (make-hash))


; Ajout d'un sommet node-id au graphe graph
(define (add-node! graph node-id)
  (when (not (hash-has-key? graph node-id))
    (hash-set! graph node-id (mutable-set))))

; Ajout arete entre sommets node-id1 et node-id2
(define (add-edge! graph node-id1 node-id2)
  (when (not (equal? node-id1 node-id2))
    (add-node! graph node-id1)
    (add-node! graph node-id2)
    (set-add! (hash-ref graph node-id1) node-id2)
    (set-add! (hash-ref graph node-id2) node-id1)))        
  
; Sommet de graph
(define (get-nodes graph)
  (hash-keys graph))

; sommets voisins
(define (get-neighbors graph node-id)
  (hash-ref graph node-id))

; suppression de node
(define (rm-node! graph node-id)
  (when (hash-has-key? graph node-id)
    (hash-remove! graph node-id)
    (for ([(k v)(in-hash graph)])
      (when (set-member? (hash-ref graph k) node-id)
        (set-remove! (hash-ref graph k) node-id)))))

; suppression d'arete
(define (rm-edge! graph node-id1 node-id2)
  (when (and (hash-has-key? graph node-id1)(hash-has-key? graph node-id2))
    (for ([(k v)(in-hash graph)])
        (when (equal? k node-id1)
          (set-remove! (hash-ref graph k) node-id2))
        (when(equal? k node-id2)
          (set-remove! (hash-ref graph k) node-id1)))))

;Affiche le graphe en montrant les listes d'adjacence
(define (print-graph graph)
  (for ([(k v)(in-hash graph)])
    (printf "~a : ~a \n" k (set->list v))))

; Application de f sur tous les nodes
(define (iter-node f graph)
  (foldl f (get-nodes graph) '()))


; Aplication de f sur toutes les aretes
(define (iter-edge f graph)
  (for ([(k v)(in-hash graph)])
   (f k v)))

; Variables de test

(define g(empty-graph))
(define v(empty-graph))


(add-edge! g 'A 'G)
(add-edge! g 'A 'C)

(add-edge! g 'D 'G)
(add-edge! g 'D 'S)

