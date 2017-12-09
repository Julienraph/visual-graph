#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt")
(provide (all-defined-out))
(require racket/random)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONSTANTES ;;;;;;;;;;;;;;;;;;

(define RED-PEN (make-object pen% "red" 8 'solid))
(define BLACK-PEN (make-object pen% "black" 1 'solid))
(define WIDTH 500)
(define HEIGHT 500)
(define graph-x 0)
(define graph-y 0)
(define g (empty-graph))
(define e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
(define r (new-relaxator))
(define animation #f)
(define dot-regex "[a-zA-Z]|[0-9]->[a-zA-Z]|[0-9]")

;;Calcul du Barycentre du graphique

(define (BARYCENTRE g e)
  (let ([sum-x 0]
        [sum-y 0]
        [barycentre (make-vect 250 250)])
    (when (not (equal? (hash-count e) 0))
      (begin
        (for ([(k v) (in-hash e)])
          (set! sum-x (+ sum-x (coord-x v)))
          (set! sum-y (+ sum-y (coord-y v))))
        (set! barycentre (make-vect (/ sum-x (hash-count e)) (/ sum-y (hash-count e))))))
    barycentre))

;; Double buffer
(define BITMAP (make-object bitmap% WIDTH HEIGHT))
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP)))

;; Horloge animation
(define TIMER (new timer% (notify-callback (lambda () (send CANVAS on-paint)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMAGES ;;;;;;;;;;;;;;;;;;;

(define pause (read-bitmap "PAUSE.png"))
(define play (read-bitmap "PLAY.png"))
(define accelerer (read-bitmap "ACCELERER.png"))
(define sauvegarder (read-bitmap "SAVE.png"))
(define zoom (read-bitmap "ZOOM.png"))
(define dezoom (read-bitmap "DEZOOM.jpg"))
(define arrete (read-bitmap "arrete.png"))
(define SupprArrete (read-bitmap "notarrete.png"))
(define bouton (read-bitmap "bouton.png"))
(define pasbouton(read-bitmap "supprboutton.png"))
(define slow (read-bitmap "SLOW.png"))
(define graphique (read-bitmap "graphique2.png"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINITIONS PRATIQUES ;;;;;;;;;;;

;; Fonction qui dessine les sommets
(define (dessiner-sommets g e)
  (for ([(k v) (in-hash e)])
    
    (send BITMAP-DC set-pen RED-PEN)
    (send BITMAP-DC draw-point (coord-x v) (coord-y v))))

;; Fonction qui dessine les liens entre les sommets
(define (dessiner-liens g e)
  (for ([(k v) (in-hash e)])
    (when (hash-has-key? g k)
      (for ((i (in-set (get-neighbors g k))))
        (when (hash-has-key? g k)
          (send BITMAP-DC set-pen BLACK-PEN)
          (send BITMAP-DC draw-line (coord-x (hash-ref e i)) (coord-y (hash-ref e i))
                (coord-x v) (coord-y v)))))))

;; Conversion de fichier DOT a liste adjacente
(define (dot->list file)
  (call-with-input-file file
    (lambda (p-in)
      (let ([res-graph (empty-graph)])
        (do ((ligne (read-line p-in)(read-line p-in)))
          ((eof-object? ligne)(void))
          (when (regexp-match dot-regex ligne)
            (add-edge! res-graph (first (string->list ligne)) (sixth (string->list ligne)))))res-graph))))

;; Conversion de liste adjacente a fichier DOT
(define (list->dot L file-name)
  (call-with-output-file file-name
    (lambda (p-out)
      (define (imprimer arc)
        (fprintf p-out "~a -> ~a ;\n" (car arc)(cadr arc)))
      (fprintf p-out "diagraph G {\n")
      (for-each imprimer L)
      (fprintf p-out "}\n"))
    #:exists 'replace))

; Conversion de graphique en liste adjacente
(define (graph->list graph)
  (let ([res null])
    (for ([(k v) (in-hash graph)])
      (set! res (append res (list(list k (set->list v))))))res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINITIONS GRAPHIQUES ;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; FRAMES ;;;;;;;;;;;;;;;;;;;;;

(define FRAME (new frame% (label "Visualisation graphique")))

(define HPANEL
  (new horizontal-panel%
       (parent FRAME)
       ))

(define VPANEL
  (new vertical-panel%
       (parent HPANEL)
       ))

(define hpanel2
  (new horizontal-panel%
       [parent VPANEL]
       [min-height 0]
       [stretchable-height #f]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; CANVAS ;;;;;;;;;;;;;;;;;;;;;

;; Dessin du graphique
(define CANVAS (new canvas%
                    (parent VPANEL)
                    (min-width WIDTH)
                    (min-height HEIGHT)
                    (paint-callback (lambda (obj dc)
                                      (send BITMAP-DC clear)
                                      (send BITMAP-DC set-smoothing 'smoothed)
                                      (dessiner-liens g e)
                                      (dessiner-sommets g e)
                                      (send dc draw-bitmap BITMAP 0 0 'solid)
                                      (r 'relax g e)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; DIALOGS ;;;;;;;;;;;;;;;;;;;;

;; Fenetre choix: Nouveau graphique ou importer
(define START-DIALOG
  (new dialog%
       (label "Visualisation graphique")
       (width 300)
       (height 100)))

;; Panel pour organiser les buttons de START-DIALOG
(define START-DIALOG-PANEL (new horizontal-panel%
                                [parent START-DIALOG]
                                [alignment '(center center)]))

;; Fenetre de creation de graphe
(define NEW-GRAPH-DIALOG
  (new dialog%
       (label "Création de graphe")
       (width 250)
       (height 200)))

(define NEW-GRAPH-DIALOG-HPANEL (new horizontal-panel%
                                     [parent NEW-GRAPH-DIALOG]
                                     [alignment '(center center)]))

(define NEW-GRAPH-DIALOG-VPANEL (new vertical-panel%
                                     [parent NEW-GRAPH-DIALOG-HPANEL]
                                     [alignment '(center center)]))

;; Objet graphique de type choice qui permet de choisir les graphiques à disposition
(define NEW-GRAPH-CHOICE
  (new choice%
       (label "Type graph: ")
       (parent NEW-GRAPH-DIALOG-VPANEL)
       (choices
        (list "" "Chain" "Cyclic" "Complete Tree" "Grid" "Clique"))
       (callback
        (lambda (obj evt)
          (when (equal? (send NEW-GRAPH-CHOICE get-string-selection) "Complete Tree")
            (send NEW-GRAPH-ARETE enable #t)
            (send NEW-GRAPH-ARETE set-value "0")
            (send NEW-GRAPH-ARETE set-label "Profondeur: ")
            (send NEW-GRAPH-TEXT set-label "Arité:"))
          (when (equal? (send NEW-GRAPH-CHOICE get-string-selection) "Grid")
            (send NEW-GRAPH-ARETE enable #t)
            (send NEW-GRAPH-ARETE set-value "0")
            (send NEW-GRAPH-TEXT set-label "Largeur: ")
            (send NEW-GRAPH-ARETE set-label "Hauteur: "))
          (when (or (equal? (send NEW-GRAPH-CHOICE get-string-selection) "Chain")
                    (equal? (send NEW-GRAPH-CHOICE get-string-selection) "")
                    (equal? (send NEW-GRAPH-CHOICE get-string-selection) "Clique")
                    (equal? (send NEW-GRAPH-CHOICE get-string-selection) "Cyclic"))
            (send NEW-GRAPH-ARETE enable #f)
            (send NEW-GRAPH-ARETE set-label "Nombre d'arêtes: ")
            (send NEW-GRAPH-ARETE set-value "")
            (send NEW-GRAPH-TEXT set-label "Nombre de sommets: "))))))
            

;; Objet graphique de type text-field qui permet de stocker le nombre de sommets choisis par l'utilisateur
(define NEW-GRAPH-TEXT
  (new text-field%
       (label "Nombre de sommets: ")
       (parent NEW-GRAPH-DIALOG-VPANEL)
       (init-value "0")))

(define NEW-GRAPH-ARETE
  (new text-field%
       (label "Nombre d'arêtes: ")
       (parent NEW-GRAPH-DIALOG-VPANEL)
       (init-value "0")
       	[enabled #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; BUTTONS ;;;;;;;;;;;;;;;;;;;;

;; Création de graphique a partir des données entrées par l'utilisateur
(define NEW-GRAPH-DIALOG-BUTTON
  (new button%
       (label "Accepter")
       (parent NEW-GRAPH-DIALOG-VPANEL)
       (callback
        (lambda (obj evt)
          (let ([type-graph (send NEW-GRAPH-CHOICE get-string-selection)]
                [nombre-sommets (string->number(send NEW-GRAPH-TEXT get-value))]
                [nombre-aretes (string->number(send NEW-GRAPH-ARETE get-value))])
            (cond
              [(equal? type-graph "Chain") (set! g (chain-graph nombre-sommets))]
              [(equal? type-graph "Cyclic") (set! g (cyclic-graph nombre-sommets))]
              [(equal? type-graph "Complete Tree") (set! g (complete-tree-graph nombre-sommets nombre-aretes))]
              [(equal? type-graph "Grid") (set! g (grid-graph nombre-sommets nombre-aretes))]
              [(equal? type-graph "Clique") (set! g (clique-graph nombre-sommets))]
              [(equal? type-graph "") (set! g (sommet-graph nombre-sommets))]
              [else (printf "Valeur: ~a" type-graph)]) ;; Todo: Faire en sorte de retourner erreur
            (when (equal? (hash-count g) 0)
              (send SUPPRESSION-SOMMET-BUTTON enable #f)
              (send AJOUTER-ARETE-BUTTON enable #f)
              (send SUPPRESSION-ARETE-BUTTON enable #f))
            (when (equal? (hash-count g) 1)
              (send AJOUTER-ARETE-BUTTON enable #f)
              (send SUPPRESSION-ARETE-BUTTON enable #f))
            (when (not (equal? type-graph (empty-graph)))
              (set! e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
              (send FRAME show #t)
              
              (send NEW-GRAPH-DIALOG show #f)))))))



;; Mise en pause de l'animation en cours
(define PAUSE-ANIMATION-BUTTON
  (new button%
       (label play)
       (parent hpanel2)
       (style '(border))
       (callback
        (lambda (obj evt)
          (if (equal? animation #t)
              (begin
                (send TIMER stop)
                (set! animation #f)
                (send PAUSE-ANIMATION-BUTTON set-label play))
              (begin
                (send TIMER start 20)
                (set! animation #t)
                (send PAUSE-ANIMATION-BUTTON set-label pause)))))))


;; Modification du graphique
(define MODIFY-GRAPH-BUTTON
  (new button%
       (label graphique)
       (parent hpanel2)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send NEW-GRAPH-DIALOG show #t)
          (send NEW-GRAPH-DIALOG-VPANEL set-label "Modification de graphe")))))


;; Button qui renvoie l'utilisateur vers une fenetre de création 
(define NEW-GRAPH-BUTTON
  (new button%
       [parent START-DIALOG-PANEL]
       [label "Nouveau graphique"]
       (callback
        (lambda (obj evt)
          (send NEW-GRAPH-DIALOG show #t)
          (send START-DIALOG show #f)))))


;; Button d'importation de fichier dot
(define IMPORT-GRAPH-BUTTON
  (new button%
       [parent START-DIALOG-PANEL]
       [label "Importer graphique"]
       [callback
        (lambda (obj evt)
          (define imported-file
            (get-file "Importer graphique" #f #f #f #f null '(("Any" "*.dot"))))
          (when (not (equal? imported-file #f))
          (set! g (dot->list imported-file))
          (set! e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
          (send FRAME show #t)
          (send START-DIALOG show #f)))]))


;; Button sauvegarde de graphe sur emplacement choisis par l'utilisateur
(define SAVE-GRAPH-BUTTON
  (new button%
       (parent hpanel2)
       (label sauvegarder)
       (style'(border))
       (callback
        (lambda (obj evt)
          (let([file-path (put-file)]
               [res-list (graph->list g)])
            (when (not (equal? file-path #f))
            (list->dot res-list file-path)))))))


;; Ajout de sommet en position aléatoire
(define AJOUTER-SOMMET-BUTTON
  (new button%
       (label bouton)
       (parent hpanel2)
       (style '(border))
       (callback
        (lambda (obj evt)
          (let ([Sommet (hash-keys g)]
                [maximum 0])
            (set! Sommet (hash-keys g))
            (if (> (length Sommet) 0)
                (begin (set! maximum (apply max Sommet))
                       (add-node! g (+ maximum 1))
                       (hash-set! e (+ maximum 1) (make-vect (random WIDTH) (random HEIGHT))))
                (begin (add-node! g 0)
                       (hash-set! e 0 (make-vect (random WIDTH) (random HEIGHT)))
                       (send SUPPRESSION-SOMMET-BUTTON enable #t)))
            (when (equal? (length Sommet) 1)
              (send AJOUTER-ARETE-BUTTON enable #t))
            (send CANVAS on-paint))))))

;; Suppression d'un sommet aleatoire de l'animation en cours
(define SUPPRESSION-SOMMET-BUTTON
    (new button%
         (label pasbouton)
         (parent hpanel2)
         (style '(border))
         (callback
          (lambda (obj evt)
            (let ([nbSommet (hash-count g)]
                  [SommetAleatoire 0])
            (when (not (equal? nbSommet 0))
              (begin (set! SommetAleatoire (random-ref (hash-keys e)))
                     (rm-node! g SommetAleatoire)
                     (hash-remove! e SommetAleatoire)
                     (send CANVAS on-paint)))
             (when (equal? nbSommet 1)
               (send SUPPRESSION-SOMMET-BUTTON enable #f))
            (when (equal? nbSommet 2)
              (send AJOUTER-ARETE-BUTTON enable #f)
              (send SUPPRESSION-ARETE-BUTTON enable #f))
            (when (equal? nbSommet 3)
              (when (set->list (first (hash-values g)))
                (send SUPPRESSION-ARETE-BUTTON enable #f))))))))
              


;; Ajout d'arete entre deux sommets aléatoires
(define AJOUTER-ARETE-BUTTON
  (let ([Sommet empty]
        [SommetAleatoire empty]
        [Filter empty]                                  
        [voisin 0])   ;; TODO: Faire un filter pour voir si le sommet n'a pas déjà tous les sommets comme voisin
    (new button%
         (label arrete)
         (parent hpanel2)
         (style '(border))
         (callback
          (lambda (obj evt)
            (set! Sommet (hash-keys g))
            (set! Filter (filter (lambda (x) (not (equal? (- (hash-count g) 1) (length (set->list (hash-ref g x)))))) (hash-keys g)))
            (when (not (equal? Filter empty))
              (set! SommetAleatoire (random-ref Filter))
              (set! voisin (set->list (hash-ref g SommetAleatoire)))
              (for ((i (in-list voisin)))
                (set! Sommet (remove i Sommet)))
              (set! Sommet (remove SommetAleatoire Sommet))
              (add-edge! g SommetAleatoire (random-ref Sommet))
              (send CANVAS on-paint)
              (send SUPPRESSION-ARETE-BUTTON enable #t)))))))


;; Suppression de arete aléatoire
(define SUPPRESSION-ARETE-BUTTON
  (let ([SommetAleatoire 0]
        [ArreteAleatoire 0]
        [Filter empty])
    (new button%
         (label SupprArrete)
         (parent hpanel2)
         (style '(border))
         (callback
          (lambda (obj evt)
            (set! Filter (filter (lambda (x) (not (equal? (mutable-set) (hash-ref g x)))) (hash-keys g)))
            (when (not (equal? Filter empty))
                 ;tester si il y a au moins un sommet qui a un voisin
              (set! SommetAleatoire (random-ref  Filter))
              (set! ArreteAleatoire (random-ref (set->list (hash-ref g SommetAleatoire))))
              (rm-edge! g SommetAleatoire ArreteAleatoire)
              (send CANVAS on-paint)
              (when (equal? (length Filter) 2)
                (send SUPPRESSION-ARETE-BUTTON enable #f))))))))






  ;; Zoom du graphique



  (define ZOOM-GRAPH-BUTTON
    (new button%
         (label zoom) ;; TODO: Remplacer par icone
         (parent hpanel2)
         (style '(border))
         (callback
          (lambda (obj evt)
            (let* ([init-matrix (send BITMAP-DC get-initial-matrix)]
                   [x (vector-ref init-matrix 0)]
                   [y (vector-ref init-matrix 3)]
                   [tx (vector-ref init-matrix 4)]
                   [ty (vector-ref init-matrix 5)])
              (send BITMAP-DC set-initial-matrix
                    (vector (+ x 0.1) 0 0 (+ y 0.1) (- tx 25) (- ty 25)))
              (send CANVAS on-paint))))))
        
          
  ;; Dezoom du graphique
  (define DEZOOM-GRAPH-BUTTON
    (new button%
         (label dezoom) ;; TODO: Remplacer par icone
         (parent hpanel2)
         (style '(border))
         (callback
          (lambda (obj evt)
            (let* ([init-matrix (send BITMAP-DC get-initial-matrix)]
                   [x (vector-ref init-matrix 0)]
                   [y (vector-ref init-matrix 3)]
                   [tx (vector-ref init-matrix 4)]
                   [ty (vector-ref init-matrix 5)])
              (send BITMAP-DC set-initial-matrix
                    (vector (- x 0.1) 0 0 (- y 0.1) (+ tx 25) (+ ty 25)))
              (send CANVAS on-paint))))))
                 
          

  ;; Ralentissement du temps de l'animation
  (define RALENTI-GRAPH-BUTTON
    (new button%
         (label slow) ;; TODO: Remplacer par icone
         (parent hpanel2)
         (style '(border))
         (callback
          (lambda (obj evt)
            (send TIMER start (inexact->exact (round (* (send TIMER interval) 1.5))))))))

  ;; Acceleration du temps de l'animation
  (define ACCELERATION-GRAPH-BUTTON
    (new button%
         (label accelerer) ;; TODO: Remplacer par icone
         (parent hpanel2)
         (style '(border))
         (callback
          (lambda (obj evt) 
            (send TIMER start (inexact->exact (+ (round (* (send TIMER interval) 0.5)) 1)))))))

  ;; Champ de texte c1
  (define C1-MODIFICATION-TEXTFIELD
    (new text-field%
         (label "C1")
         (parent FRAME)
         (init-value (number->string(r 'get-c1)))))

  ;; Champ de texte c1
  (define C2-MODIFICATION-TEXTFIELD
    (new text-field%
         (label "C2")
         (parent FRAME)
         (init-value (number->string(r 'get-c2)))))

  ;; Champ de texte c1
  (define C3-MODIFICATION-TEXTFIELD
    (new text-field%
         (label "C3")
         (parent FRAME)
         (init-value (number->string(r 'get-c3)))))
       
  ;; Application des modification des constantes c1, c2 et c3
  (define MODIFICATION-CONSTANTES-BUTTON
    (new button%
         (label "Appliquer")
         (parent FRAME)
       
         (style '(border))
         (callback
          (lambda (obj evt)
            (let ([c1 (string->number (send C1-MODIFICATION-TEXTFIELD get-value))]
                  [c2 (string->number (send C2-MODIFICATION-TEXTFIELD get-value))]
                  [c3 (string->number (send C3-MODIFICATION-TEXTFIELD get-value))])
              (begin
                (r 'set-c1 c1)
                (r 'set-c2 c2)
                (r 'set-c3 c3)))))))


  (send START-DIALOG show #t)