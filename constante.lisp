;;;;;;;;;;;;;;;;;;;;;;;;;Sudoku.lisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Ce fichier contient les constantes du programme.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fichier contenant les constantes du programme


;; taille initial des grille de jeux
(defparameter +SIZE+ 9)                    ; longueur d'un coté de la grille
(defparameter +AREA+ (* +SIZE+ +SIZE+))    ; aire du la grille (nombre de case)
(defparameter +CARRE-SIZE+ 3)              ; taille des sous carrés

;;grille initial du jeux

(defparameter +grid+ 
  (make-array '(9 9) :initial-contents
	      '((1 0 0 0 0 4 0 0 5)
		(0 0 0 9 5 0 0 8 0)
		(0 0 0 0 0 3 0 9 0)
		(0 0 5 0 0 2 0 0 4)
		(0 0 1 0 6 0 7 0 0)
		(7 0 0 3 0 0 2 0 0)
		(0 6 0 5 0 0 0 0 0)
		(0 8 0 0 1 6 0 0 0)
		(5 0 0 2 0 0 0 0 7))))

(defparameter +grid-test+
  (make-array '(9 9) :initial-contents
	      '((9 2 0 0 0 0 0 3 8)
		(0 0 0 5 0 0 7 2 0)
		(0 0 1 2 0 0 0 0 6)
		(5 9 0 6 0 0 3 0 0)
		(1 3 0 9 0 5 0 6 4)
		(0 0 7 0 0 4 0 9 2)
		(7 0 0 0 0 6 2 0 0)
		(0 4 8 0 0 9 0 0 0)
		(2 5 0 0 0 0 0 8 7))))

(defparameter +possible-grid-test+
  (make-array '(9 9) :initial-contents
	      '((NIL NIL (4 5 6) (1 4 7) (1 4 6 7) (1 7) (1 4 5) NIL NIL)
		((3 4 6 8) (6 8) (3 4 6) NIL (1 3 4 6 8 9) (1 3 8) NIL NIL (1 9))
		((3 4 8) (7 8) NIL NIL (3 4 7 8 9) (3 7 8) (4 5 9) (4 5) NIL)
		(NIL NIL (2 4) NIL (1 2 7 8) (1 2 7 8) NIL (1 7) (1))
		(NIL NIL (2) NIL (2 7 8) NIL (8) NIL NIL)
		((6 8) (6 8) NIL (1 3 8) (1 3 8) NIL (1 5 8) NIL NIL)
		(NIL (1) (3 9) (1 3 4 8) (1 3 4 5 8) NIL NIL (1 4 5) (1 3 5 9))
		((3 6) NIL NIL (1 3 7) (1 2 3 5 7) NIL (1 5 6) (1 5) (1 3 5))
		(NIL NIL (3 6 9) (1 3 4) (1 3 4) (1 3) (1 4 6 9) NIL NIL))))
