#lang racket/gui

(require "relaxation.rkt" "positioning.rkt" "vect2D.rkt" "graph.rkt" "graph-generators.rkt")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINITIONS GRAPHIQUES ;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; FRAMES ;;;;;;;;;;;;;;;;;;;;;

(define FRAME (new frame% (label "Visualisation graphique")))

(define HPANEL
  (new horizontal-panel%
       (parent FRAME)))

(define VPANEL
  (new vertical-panel%
       (parent HPANEL)))

(define hpanel2
  (new horizontal-panel%
       [parent VPANEL]))


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
                                      (send dc draw-bitmap BITMAP graph-x graph-y 'solid)
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

(define NEW-GRAPH-CHOICE
  (new choice%
       (label "Type graph: ")
       (parent NEW-GRAPH-DIALOG-VPANEL)
       (choices
        (list "" "Chain" "Cylic" "Complete Tree" "Grid" "Clique"))))

(define NEW-GRAPH-TEXT
  (new text-field%
       (label "Nombre de sommets: ")
       (parent NEW-GRAPH-DIALOG-VPANEL)
       (init-value "0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; BUTTONS ;;;;;;;;;;;;;;;;;;;;

(define NEW-GRAPH-DIALOG-BUTTON
  (new button%
       (label "Accepter")
       (parent NEW-GRAPH-DIALOG-VPANEL)
       (callback
        (lambda (obj evt)
          (let ([type-graph (send NEW-GRAPH-CHOICE get-string-selection)]
                [nombre-sommets (string->number(send NEW-GRAPH-TEXT get-value))])
            (cond
              [(equal? type-graph "Chain") (set! g (chain-graph nombre-sommets))]
              [(equal? type-graph "Cylic") (set! g (cyclic-graph nombre-sommets))]
              [(equal? type-graph "Complete Tree") (set! g (complete-tree-graph nombre-sommets 2))] ; (?)
              [(equal? type-graph "Grid") (set! g (grid-graph nombre-sommets nombre-sommets))] ; lol
              [(equal? type-graph "Clique") (set! g (clique-graph nombre-sommets))]
              [else (printf "Valeur: ~a" type-graph)]) ;; Todo: Faire en sorte de retourner erreur
            (when (not (equal? type-graph (empty-graph)))
              (set! e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
              (send FRAME show #t)
              (send NEW-GRAPH-DIALOG show #f)))))))




(define PAUSE
  (new button%
       (label "Start")
       (parent hpanel2)
       (style '(border))
       (callback
        (lambda (obj evt)
          (if (equal? animation #t)
              (begin
                (send TIMER stop)
                (set! animation #f)
                (send PAUSE set-label play))
              (begin
                (send TIMER start 20)
                (set! animation #t)
                (send PAUSE set-label pause)))))))



(define SUPPRESSION-SOMMET-BUTTON
  (let ([SommetAlea 0])
    (new button%
         (label "Enlever sommet")
         (parent HPANEL)
         (style '(border))
         (callback
          (lambda (obj evt)
            (when (not (equal? (hash-count g) 0))
              (begin (set! SommetAlea (random-ref (hash-keys e)))
                     (rm-node! g SommetAlea)
                     (hash-remove! e SommetAlea)
                     (send CANVAS on-paint))))))))
          
  

;enlever le bouton quand length=1

;; Button modification de graphe
(define MODIFY
  (new button%
       (label "Modifier")
       (parent hpanel2)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send NEW-GRAPH-DIALOG show #t)
          (send NEW-GRAPH-DIALOG-VPANEL set-label "Modification de graphe")))))

;; Button de creation de graphe
(define new-graph-button
  (new button%
       [parent START-DIALOG-PANEL]
       [label "Nouveau graphique"]
       (callback
        (lambda (obj evt)
          (send NEW-GRAPH-DIALOG show #t)
          (send START-DIALOG show #f)))))


;; Button d'importation de fichier dot
(define import-graph-button
  (new button%
       [parent START-DIALOG-PANEL]
       [label "Importer graphique"]
       [callback
        (lambda (obj evt)
          (define imported-file
            (get-file "Importer graphique" #f #f #f #f null '(("Any" "*.dot"))))
          (set! g (dot->list imported-file))
          (set! e (random-positioning-of-node-list WIDTH HEIGHT (get-nodes g)))
          (send FRAME show #t)
          (send START-DIALOG show #f))]))

;; Button sauvegarde de graphe
(define SAVE-BUTTON
  (new button%
       (parent hpanel2)
       (label "Sauvegarder")
       (style'(border))
       (callback
        (lambda (obj evt)
          (let([file-path (put-file)]
               [res-list (graph->list g)])
            
            (list->dot res-list file-path))))))


(define AddArrete
  (let ([Sommet empty]
        [SommetAlea empty]
        [IsFilter empty]
                                   
        [voisin 0])   ;Faire un filter pour voir si le sommet n'a pas déjà tous les sommets comme voisin
    (new button%
         (label "AddArrete")
         (parent HPANEL)
         (style '(border))
         (callback
          (lambda (obj evt)
            (set! Sommet (hash-keys g))
            (set! IsFilter (filter (lambda (x) (not (equal? (- (hash-count g) 1) (length (set->list (hash-ref g x)))))) (hash-keys g)))
            (when (not (equal? IsFilter empty))
              (set! SommetAlea (random-ref IsFilter))
              (set! voisin (set->list (hash-ref g SommetAlea)))
              (for ((i (in-list voisin)))
                (set! Sommet (remove i Sommet)))
              (set! Sommet (remove SommetAlea Sommet))
              (add-edge! g SommetAlea (random-ref Sommet))
              (send CANVAS on-paint)))))))

(define AddSommet
  (new button%
       (label "AddSommet")
       (parent HPANEL)
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
                       (hash-set! e 0 (make-vect (random WIDTH) (random HEIGHT)))))
            (send CANVAS on-paint))))))
          
          


;random-ref?





(define REMOVEArr
  (let ([SommetAlea 0]
        [ArreteAlea 0]
        [IsFilter empty]
        )
    (new button%
         (label "removearr")
         (parent HPANEL)
         (style '(border))
         (callback
          (lambda (obj evt)
            (set! IsFilter (filter (lambda (x) (not (equal? (mutable-set) (hash-ref g x)))) (hash-keys g)))
            (when (not (equal? IsFilter empty))
              ;tester si il y a au moins un sommet qui a un voisin
              (set! SommetAlea (random-ref  IsFilter))
              (set! ArreteAlea (random-ref (set->list (hash-ref g SommetAlea))))
              (rm-edge! g SommetAlea ArreteAlea)
              (send CANVAS on-paint)))))))

(define zoom
  (new button%
       (label "zoom")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send BITMAP-DC scale 1.5 1.5)
          ))))
;jpp
         
         

(define dezoom
  (new button%
       (label "dezoom")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send BITMAP-DC scale 0.5 0.5)))))

(define ralenti
  (new button%
       (label "ralenti")
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt)
          (send TIMER start (inexact->exact (round (* (send TIMER interval) 1.5))))))))

(define accèlerer
  (new button%
       (label accelerer)
       (parent HPANEL)
       (style '(border))
       (callback
        (lambda (obj evt) 
          (send TIMER start (inexact->exact (+ (round (* (send TIMER interval) 0.5)) 1)))))))

(define modifier
  (new text-field%
       (label "modifier")
       (parent FRAME)
       (init-value "0")))
       

(define modifierbutton
  (new button%
       (label "Modifier")
       (parent FRAME)
       
       (style '(border))
       (callback
        (lambda (obj evt)
          (let ((n (string->number (send modifier get-value))))
            (r 'set-c1 n))))))




;(define reinitialisation
;  (new button%
;       (label "reinitialisation")
;       (parent HPANEL)
;       (style '(border))
;       (callback
;        (lambda (obj evt)
;          (set! g graphtempo)))))

(send START-DIALOG show #t)