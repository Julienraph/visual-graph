#lang racket
;; Module positioning.rkt pour projet "Visualisation de graphe"
;; DrRacket 6.10

(require "vect2D.rkt")
(provide (all-defined-out))

;; Les fonctions de placement que l'on va utiliser utiliseront des donné de type
;; table de hachage. La raison pour celle ci est la facilité d'acces aux vecteurs
;; grace au noeud(Keys/values). De meme, lorsque le nombre de noeud est grand la complexité pour acceder
;; aux valeurs est de O(1).

(define (positioning)(make-hash))

;; Application d'une fonction de placement à node-id de valeur vect.
(define (apply-positioning positioning node-id vect)
  (hash-set! positioning node-id vect))

;; Creation d'une fonction de placement a partir d'une liste adjacente L.
(define(positioning-of-assoc-list L)
  (let ([res-hash (make-hash)])
    (for-each (lambda (arg)
                (apply-positioning res-hash (car arg) (cdr arg))) L)res-hash))

;; Creation d'une fonction de placement d'un canevas hxw a partir des noeuds de L.
(define (random-positioning-of-node-list w h L)
  (let ([res-hash (make-hash)])
    (for-each (lambda (arg)
                (apply-positioning res-hash arg (make-vect (random  w)(random  h))))L)res-hash))
  
;; Deplacement du sommet node-id.
(define (positioning-move-node! positioning node-id vect)
  (apply-positioning positioning node-id (vect-sum (hash-ref positioning node-id) vect)))

;; Methode qui affiche a la console la fonction de placement.
(define (print-positioning id-list positioning)
  (for [(i id-list)]
     (printf "~a (~a, ~a) \n" i (car (hash-ref positioning i)) (car (cdr (hash-ref positioning i))))))




;;;;;;;;;;;;;;;;;;;;;;;;;; TEST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; ZONE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A-table ((1 (3 4) 2 (5 6))
(define L (list (list 1 (list 3 4)) (list 2 (list 5 6))))

; Liste de sommets
(define n (list 1 2 3 4))

; Fonction de placement
(define e (positioning))

; Vecteur test 1
(define t1 (make-vect 3 2))

;; Test de placement
(apply-positioning e 'a t1)

;; Creationg de vecteur test 2
(define t2 (make-vect 7 8))