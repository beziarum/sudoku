
;;; Fichier contenant les constantes du programme


;; taille initial des grille de jeux
(defparameter +SIZE+ 9)
(defparameter +AREA+ (* +SIZE+ +SIZE+))
(defparameter +CARRE-SIZE+ 3)

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

(defparameter +grid-copy+
  (make-array '(9 9) :initial-contents
	      '((5 0 7 0 0 0 0 1 0)
		(0 0 0 0 4 0 0 0 6)
		(4 6 2 0 8 0 0 0 0)
		(1 0 0 0 3 2 0 0 0)
		(0 8 6 0 0 0 2 4 0)
		(0 0 0 6 5 0 0 0 3)
		(0 0 0 0 9 0 3 6 1)
		(7 0 0 0 1 0 0 0 0)
		(0 5 0 0 0 0 7 0 4))))

(defparameter +grid-facile+
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
