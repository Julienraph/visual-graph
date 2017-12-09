#lang racket

(require rackunit "../vect2D.rkt" "../graph.rkt" "../graph-generators.rkt")
(require rackunit rackunit/gui)

(define v1 (make-vect 5 8))
(define v2 (make-vect 3 4))
(define v3 (make-vect 10 13))

(test/gui
 (test-suite "VECT2D.rkt"

             (test-suite "soit v1 (make-vect 5 8), v2 (make-vect 3 4) et v3 (make-vect 10 13)")
               
             (test-suite "make-vect"
                         (test-case "(make-vect 5 8) est-il bien égal à " (check-equal? (make-vect 5 8) '(5 . 8))))

             (test-suite "coord-x"
                         (test-case "(coord-x v1) est égal à 5?" (check-equal? (coord-x v1) 5)))

             (test-suite "coord-y"
                           (test-case "(coord-y v1) est égal à 8?" (check-equal? (coord-y v1) 8)))

             (test-suite "vect-sum"
                           (test-case "(vect-sum v1 v2) est bien égal à (8 . 12) ?" (check-equal? (vect-sum v1 v2) (make-vect 8 12))))
             (test-suite "vect-mult"
                           (test-case "(vect-mult v1 v2) est bien égal à (15 . 32) ?" (check-equal? (vect-mult v1 v2) (make-vect 15 32))))

             (test-suite "vect-sum*"
                           (test-case "(vect-sum v1 v2 v3) est t-il bien égal à (18 . 25) ?" (check-equal? (vect-sum* v1 v2 v3) (make-vect 18 25))))

             (test-suite "vect-scalar"
                         (test-case "(vect-scalar 5 v1) est t-il bien égal à (25 . 40) ?" (check-equal? (vect-scalar 5 v1) (make-vect 25 40))))

             (test-suite "vect-norm"
                         (test-case "(vect-norm v2) est t-il bien égal à 5 ?" (check-equal? (vect-norm v2) 5)))
             (test-suite "vect-unit"
                         (test-case "(vect-unit v2) est-il bien égal à (3/5 . 4/5)?" (check-equal? (vect-unit v2) (make-vect (/ 3 5) (/ 4 5))))))
                           
                         









 
 (test-suite "graph.rkt"
             
             (test-suite "add-node!"
                         (let ([g (empty-graph)]
                               [g2 (hash 'A (mutable-set))])
                           (test-case "ajoute A dans g"(add-node! g 'A))
                           (test-case "regarde si la clé A existe dans g" (check-true (hash-has-key? g 'A)))
                           (test-case "regarde si la valeur de la clé A est égal à (mutable-set)"(check-equal? (hash-ref g 'A) (hash-ref g2 'A))) ))

             (test-suite "add-edge!"
                        
                          (let ([g (empty-graph)])
                            (test-case "ajouter A dans g" (add-node! g 'A))   
                            (test-case "Ajouter arrête entre A et B alors que B n'existe pas"
                                       (add-edge! g 'A 'B))
                            (test-case "B est il une clé de g?" (check-true (hash-has-key? g 'B)))
                            (test-case "B est un voisin de A?" (check-true (set-member? (hash-ref g 'A) 'B)))
                            (test-case "A est un voisin de B?" (check-true (set-member? (hash-ref g 'B) 'A)))
                            (test-case "Faire une arrête entre deux sommmets C D qui n'existe pas" (add-edge! g 'C 'D))
                            (test-case "C est il une clé de g?" (check-true (hash-has-key? g 'C)))
                            (test-case "D est il une clé de g?" (check-true (hash-has-key? g 'D)))
                            (test-case "D est un voisin de C?" (check-true (set-member? (hash-ref g 'C) 'D)))
                            (test-case "C est un voisin de D?" (check-true (set-member? (hash-ref g 'D) 'C))) ))

             (test-suite "get-nodes"
                         (let ([g (empty-graph)])
                           (test-case "Ajouter A B C dans g" (add-edge! g 'A 'B) (add-edge! g 'C 'A) (add-edge! g 'B 'C))
                           (test-case "A est il dans la liste?" (check-true (not (equal? (member 'A (get-nodes g)) #f))))
                           (test-case "B est il dans la liste?" (check-true (not (equal? (member 'B (get-nodes g)) #f))))
                           (test-case "C est il dans la liste?" (check-true (not (equal? (member 'C (get-nodes g)) #f))))
                           (test-case "il y a aucun autre sommet" (check-true (equal? (length (get-nodes g)) 3))) ))
             
             (test-suite "get-neighbors"
                         (let ([g (empty-graph)])
                           (test-case "Ajouter arrête entre A et B,C" (add-edge! g 'A 'B) (add-node! g 'C))
                           (test-case "Get-neighbors a t-il bien tous les voisins?" (check-equal? (get-neighbors g 'A) (mutable-set 'B)))
                           (test-case "Get-neighbors a t-il bien tous les voisins?" (check-equal? (get-neighbors g 'C) (mutable-set)))
                           (test-case "Get-neighbors a t-il bien tous les voisins?" (check-equal? (get-neighbors g 'B) (mutable-set 'A))) ))
             
             (test-suite "rm-node!"
                         (let ([g (empty-graph)])
                           (test-case "Ajouter arrête entre A et B C" (add-edge! g 'A 'B) (add-edge! g 'A 'C))
                           (test-case "On fait (rm-node! g 'A)" (rm-node! g 'A))
                           (test-case "(rm-node! g 'A) a t-il bien enlevé A ?" (check-true (not(hash-has-key? g 'A))))
                           (test-case "On refait (rm-node! g 'A) pour voir si ça nous renvoie pas une erreur que la clé n'existe pas" (rm-node! g 'A))
                           (test-case "On regarde si 'A n'est plus voisin d'un autre sommet" (check-equal? (get-neighbors g 'C) (get-neighbors g 'B) ))))
             
             (test-suite "rm-edge!"
                         (let ([g (empty-graph)])
                           (test-case "Ajouter arrête entre AB, AC et BD"  (add-edge! g 'A 'B) (add-edge! g 'A 'C) (add-edge! g 'B 'D))
                           (test-case "On fait (rm-edge! g 'A 'B)" (rm-edge! g 'A 'B))
                           (test-case "On regarde si B a seulement D comme voisin" (check-equal? (get-neighbors g 'B) (mutable-set 'D)))
                           (test-case "On regarde si A a seulement C comme voisin" (check-equal? (get-neighbors g 'A) (mutable-set 'C)))
                           (test-case "On regarde si C a toujours A comme voisin" (check-equal? (get-neighbors g 'C) (mutable-set 'A)))
                           (test-case "On refait (rm-edge! g 'A 'B) pour voir si ça nous renvoie pas une erreur" (rm-edge! g 'A 'B))
                           (test-case "On regarde si A est toujours une clé de g" (check-true (hash-has-key? g 'A)))))

             )


 (test-suite "graph-generators"

             (test-suite "chain-graph"
                         (let ([g (chain-graph 10)]
                               [compteur 0])
                           (test-case "Soit g = (chain-graph 10), y a t-il bien 10 sommets?" (check-equal? (hash-count g) 10))
                           (test-case "Soit un compteur initialisé à 0, on compte les arrêtes" (for ([(k v)(in-hash g)])
                                                                                                 (for ([t (in-list (set->list v))])
                                                                                                 (set! compteur (+ compteur 1))
                                                                                                 (rm-edge! g k t))))
                           (test-case "Y a t-il bien 9 arrêtes ?" (check-equal? compteur 9))))

             (test-suite "cyclic-graph"
                         (let ([g (cyclic-graph 10)]
                               [compteur 0])
                           (test-case "Soit g = (cyclic-graph 10), y a t-il bien 10 sommets?" (check-equal? (hash-count g) 10))
                           (test-case "Soit un compteur initialisé à 0, on compte les arrêtes" (for ([(k v)(in-hash g)])
                                                                                                 (for ([t (in-list (set->list v))])
                                                                                                 (set! compteur (+ compteur 1))
                                                                                                 (rm-edge! g k t))))
                           (test-case "Y a t-il bien 10 arrêtes ?" (check-equal? compteur 10))))
             (test-suite "complete-tree-graph"
                         (let ([g (complete-tree-graph 3 2)]
                               [compteur 0])
                           (test-case "Soit g = (complete-tree-graph 3 2), y a t-il bien 13 sommets?" (check-equal? (hash-count g) 13))
                           (test-case "Soit un compteur initialisé à 0, on compte les arrêtes" (for ([(k v)(in-hash g)])
                                                                                                 (for ([t (in-list (set->list v))])
                                                                                                 (set! compteur (+ compteur 1))
                                                                                                 (rm-edge! g k t))))
                           (test-case "Y a t-il 12 arrêtes?" (check-equal? compteur 12))))

             (test-suite "grid-graph"
                         (let ([g (grid-graph 2 2)]
                               [compteur 0])
                           (test-case "Soit g = (grid-graph 2 2), y a t-il bien 9 sommets?" (check-equal? (hash-count g) 9))
                           (test-case "Soit un compteur initialisé à 0, on compte les arrêtes" (for ([(k v)(in-hash g)])
                                                                                                 (for ([t (in-list (set->list v))])
                                                                                                 (set! compteur (+ compteur 1))
                                                                                                 (rm-edge! g k t))))
                           (test-case "Y a t-il bien 12 arrêtes ?" (check-equal? compteur 12))))
             (test-suite "clique-graph"
                         (let ([g (clique-graph 10)]
                               [compteur 0])
                           (test-case "Soit g = (clique-graph 10), y a t-il bien 10 sommets ?" (check-equal? (hash-count g) 10))
                           (test-case "Soit un compteur initialisé à 0, on compte les arrêtes" (for ([(k v)(in-hash g)])
                                                                                                 (for ([t (in-list (set->list v))])
                                                                                                 (set! compteur (+ compteur 1))
                                                                                                 (rm-edge! g k t))))
                           (test-case "Y a t-il bien 10(10 - 1)/2 arrêtes ?" (check-equal? compteur (/ (* 10 (- 10 1)) 2))))))

 (test-suite "relaxation.rkt")
 )