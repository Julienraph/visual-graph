#lang racket
;;; vect2D.rkt
;;; DrRacket 6.10
;;; Module de manipulation de vecteurs 2D
;;; Pour le projet "Visualisation de graphe"

(provide (all-defined-out))

; Creation d'un vecteur 2D
(define (make-vect x y)
  (cons x y))

; x
(define (coord-x v)
  (car v))
; y
(define (coord-y v)
  (cdr v))

; Somme de deux vecteurs
(define (vect-sum v1 v2)
  (make-vect (+ (coord-x v1) (coord-x v2))
             (+ (coord-y v1) (coord-y v2))))

; Multiplication de deux vecteurs
(define (vect-mult v1 v2)
  (make-vect (* (coord-x v1) (coord-x v2))
             (* (coord-y v1) (coord-y v2))))

; Somme de n vecteurs
(define (vect-sum* . vn)
(make-vect (apply + (map (lambda(vect)
                           (coord-x vect))vn))
           (apply + (map (lambda(vect)
                           (coord-y vect))vn))))

; Multiplication d'un vecteur par un scalaire k
(define (vect-scalar k v)
  (make-vect (* k (coord-x v))
             (* k (coord-y v))))
  
; Normalisation d'un vecteur
(define (vect-norm v)
  (sqrt (+ (sqr (coord-x v))
           (sqr (coord-y v)))))

; Vecteur unitaire
(define (vect-unit v)
  (make-vect (/ (coord-x v) (vect-norm v))
             (/ (coord-y v) (vect-norm v))))