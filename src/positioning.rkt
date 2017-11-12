#lang racket
(require "vect2D.rkt" "graph.rkt")


;; On decide d'utiliser des hash tables pour stocker le positioning
(define (apply-positioning positioning node-id vect)
  (hash-set! positioning node-id vect))


(define(positioning-of-assoc-list L)
  (let ([res-hash (make-hash)])
    (for-each (lambda (arg)
                (apply-positioning res-hash (car arg) (cdr arg))) L)res-hash))
; TODO
(define (random-positioning-of-node-list w h L)
  (let ([res-hash (make-hash)])
    (for-each (lambda (arg)
                (apply-positioning res-hash arg (cons (random  h)(random  h))))L)res-hash))
  
  
(define (print-positioning id-list positioning)
  (for [(i id-list)]
     (printf "~a (~a, ~a) \n" i (car (hash-ref positioning i)) (car (cdr (hash-ref positioning i))))))

(define (positioning-move-node! positioning node-id vect)
  (apply-positioning positioning node-id vect))


;Variables de test
; A-table ((1 (3 4) 2 (5 6))
(define L (list (list 1 (list 3 4)) (list 2 (list 5 6))))
(define n (list 1 2 3 4))