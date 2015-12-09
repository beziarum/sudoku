(load "affichage")
(load "lecture")
(load "strategies")
(load "strategy2")
(load "constante")
(defparameter +SIZE+ 9)
;;Effectue une copie d'une grille de jeux


(defun grid-copy (grid)
  (let ((g (make-array '(+SIZE+ +SIZE+))))
    (loop for i below +SIZE+
	  do (loop for j below +SIZE+
		   do (setf (aref g i j)(aref grid i j))))
    g))

;;test si la valeur est deja presente dans la colonne

(defun test-colonne(grid c valeur)
  (loop for i below +SIZE+ never (eq(aref grid i c)valeur)
	finally (return T)))

;;test si la valeur est deja presente dans la ligne

(defun test-ligne(grid l valeur)
  (loop for i below +SIZE+ never (eq (aref grid l i) valeur)
	finally (return T)))

;;test si la valeur est deja presente dans le carré

(defun test-carre(grid c l valeur)
  (let ((x (- l (mod l +CARRE-SIZE+)))
	(y (- c (mod c +CARRE-SIZE+))))
    (loop for i from x below (+ x +CARRE-SIZE+)
	  always (loop for j from y below (+ y +CARRE-SIZE+)
		       never (eq(aref grid i j)valeur)
		       finally(return T)))))


;; vérifie si une valeur est permise par les regle du jeux

(defun test-valeur (grid c l valeur)
  (if (and (test-ligne grid l valeur)
	   (test-colonne grid c valeur)
	   (test-carre grid c l valeur)
	   (eq(aref grid l c) 0)
	   (and(< valeur 10)(> valeur 0)))
      T
      NIL))

;;attribut une valeur a la grille

(defun set-valeur(grid c l valeur)
  (setf(aref grid l c)valeur))


;;vérifie si on peut retirer la valeur (faut si valeur présente dans la grille de base)

(defun test-delete-valeur(grid grid-copy c l)
  (if (and
       (zerop(aref grid-copy l c))
       (not (zerop (aref grid l c))))
      T
      NIL))

;; réatribut 0 a la case

(defun delete-valeur (grid c l)
  (setf(aref grid l c)0))


;; fonction qui gére le programme a chaque tour

(defun play (grid grid-copy)
  (let ((c (progn
	     (format t "Choix de valeur (valeur 0 = effacer)~%")
	     (format t "COL LIG VAL?~% ")
	     (read))) 
	(l  (read))
	(valeur  (read)))
    (setf c (transformation c)) ;transforme la lettre de la colone en chiffre (voir fichier lecture)
    (decf l)
    (if (zerop valeur)
	(if (test-delete-valeur grid grid-copy c l)
	    (delete-valeur grid c l)
	    (progn
	      (format t "Impossible de supprimer cette valeur~% ")
	      (play grid grid-copy)))
	(if (test-valeur grid c l valeur)
	    (set-valeur grid c l valeur)
	    (progn
	      (format t"Impossible d'atribuer cette valeur a cette case ~% ")
	      (play grid grid-copy))))))



;; vérifie si une grille est finit d'etre remplit

(defun win (grid)
  (let ((c 0))
    (loop for i below +SIZE+
	  do (loop for j below +SIZE+
		   do (if (not(eq(aref grid i j)0))
			  (incf c))))
    (if (eq c +AREA+))
	T
	NIL)))

;; fonction main

(defun sudoku-main (grid g)
  (afficher-sudoku grid)
  (play grid g)
  (if (win +grid+)
      (princ "vous avez gagné")
      (sudoku-main grid g)))

;;fonction qui fait la copie de la grid et lance le main

(defun sudoku(grid)
  (let ((g (grid-copy grid )))
    (sudoku-main grid g)))


