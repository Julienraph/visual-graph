#lang racket
;;; Graph-generators.rkt
;;; DrRacket 6.10
;;; Projet "Visualisation de graphe"
(require "positioning.rkt" "vect2D.rkt" "graph.rkt")

(provide (all-defined-out))

  
;Graphe à n sommet et n-1 arêtes formant une chaîne ouverte
(define (chain-graph n)
  (let ([res-graph (empty-graph)])
  (if (equal? n 1)
      (add-node! res-graph 0)
  (for ([i (in-range (- n 1))])
    (add-edge! res-graph i (+ i 1))))
    res-graph))

;Graphe à n sommet et n arêtes formant un cycle
(define (cyclic-graph n)
  (let ([res-graph (empty-graph)])
    (if (equal? n 1)
        (add-node! res-graph 0)
    (for ([i (in-range (- n 1))])
          (when (< i (- n 1))
              (add-edge! res-graph i (+ i 1)))
            (add-edge! res-graph (- n 1) 0)))
              res-graph))


;Graphe d'un arbre complet d'arité arity et profondeur depth
(define (complete-tree-graph arity depth)
  (define (NombreBoucle arity depth) ;Nombre de boucle à faire, aussi égal au nombre de sommet de l'arbre qui a (arity - 1) sommet
  (if (equal? 0 depth)
      0
      (+ (expt arity (- depth 1)) (NombreBoucle arity (- depth 1)))))
  (let ([res-graph (empty-graph)]
        [depart 1])
    (for ([i (in-range (NombreBoucle arity depth))])
      (for ([j (in-range depart (+ depart arity))])
      (add-edge! res-graph i j))
      (set! depart (+ depart arity)))
     res-graph))


;Graphe d'une grille n*m
(define (grid-graph n m)
  (let ([res-graph (empty-graph)])
    (for ([i (in-range (+ m 1))])
      (for ([j (in-range (+ n 1))])
        (when (< j n)
        (add-edge! res-graph (+ (* (+ n 1) i) j) (+ (+ (* (+ n 1) i) j) 1))) ;ajoute le sommet de droite si il n'est pas au bord à droite
        (when (< i m)
          (add-edge! res-graph (+ (* (+ n 1) i) j) (+ (+ (* (+ n 1) i) j) (+ n 1)))))) ;ajoute le sommet du bas si il n'est pas au bord de la grille en bas
    res-graph))
    

;Clique à n Sommets, c'est à dire tous les sommets reliés entre eux
(define (clique-graph n)
  (let ([res-graph (empty-graph)])
    (if (equal? n 1)
        (add-node! res-graph 0)
    (for ([i (in-range n)])
      (for ([j (in-range 1 n)])
        (add-edge! res-graph i j))))
      res-graph))