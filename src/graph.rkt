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
  (hash-set! graph node-id '()))

; TODO: Verifier qu'il existe node-id1 et node-id2
; S'il existe pas on cree les deux sommets avec une arete
; Si l'arete existe deja on modifie pas le graphe
; Probleme: Comment parcourir le graphe pour verifier
; l'existence d'un sommet specifique? 
(define (add-edge! graph node-id1 node-id2)
  (hash-set! graph node-id1 (cons node-id2 empty))
  (hash-set! graph node-id2 (cons node-id1 empty)))

;;TODO
(define (add-edge g n1 n2)
  (cond
    [(and (hash-has-key? g n1) (hash-has-key? g n2))(begin
                                                      (hash-set! g n1 (cons (hash-ref g n1) n2))
                                                      (hash-set! g n2 (cons (hash-ref g n2) n1)))]))
    
  

; Sommet de graph
; TODO: Le type de donné a renvoyer doit etre une liste
(define (get-nodes graph)
  (for ([(k v)(in-hash graph)])
    (printf "~a " k)))

; sommets voisins
; ???
(define (get-neighbors graph node-id)
  (hash-ref graph node-id))


; Variables de test

(define g(empty-graph))
(define v(empty-graph))