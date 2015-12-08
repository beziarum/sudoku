(load "affichage")
(load "lecture")
(load "strategies")
(load "strategy2")

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


;;effectue une copie d'une grille de jeux


(defun grid-copy (grid)
  (let ((g (make-array '(9 9))))
    (loop for i below 9
       do (loop for j below 9
	     do (setf (aref g i j)(aref grid i j))))
    g))

;;test si la valeur est deja presente dans la colone

(defun test-colonne(grid c valeur)
  (loop for i below +size+ never (eq(aref grid i c)valeur)
       finally (return T)))

;;test si la valeur est deja presente dans la ligne

(defun test-ligne(grid l valeur)
  (loop for i below +size+ never (eq (aref grid l i) valeur)
      finally (return T)))

;;test si la valeur est deja presente dans le carré

(defun test-carre(grid c l valeur)
  (let ((x (- l (mod l +CARRE-SIZE+)))
       (y (- c (mod c +CARRE-SIZE+))))
    (loop for i from x to (-(+ x +CARRE-SIZE+)1) 
       always (loop for j from y to (-(+ y +CARRE-SIZE+)1) never (eq(aref grid i j)valeur)
       finally(return T)))))


(defun test-valeur (grid c l valeur)
  (if (and (test-ligne grid l valeur)
	   (test-colonne grid c valeur)
	   (test-carre grid c l valeur)
	   (eq(aref grid l c) 0)
	   (and(< valeur 10)(> valeur 0)))
      T
      NIL))



(defun set-valeur(grid c l valeur)
  (setf(aref grid l c)valeur))


(defun test-delete-valeur(grid grid-copy c l)
  (if (and (zerop(aref grid-copy l c)) (not (zerop (aref grid l c))))
      T
      NIL))

(defun delete-valeur (grid c l)
  (setf(aref grid l c)0))



(defun play (grid grid-copy)
  (let ((c (progn 
	     (princ "COL LIG VAL? ")
	     (read)))
	(l  (read))
	(valeur  (read)))
    (setf c (transformation c))
    (decf l)
    (if (zerop valeur)
	(if (test-delete-valeur grid grid-copy c l)
	    (delete-valeur grid c l)
	    (progn
	      (princ "Impossible de supprimer cette valeur ")
	      (play grid grid-copy)))
	(if (test-valeur grid c l valeur)
	    (set-valeur grid c l valeur)
	    (progn
	      (princ "Impossible d'atribuer cette valeur a cette case ")
	      (play grid grid-copy))))))



(defun win (grid)
  (let ((c 0))
  (loop for i below +SIZE+
       do (loop for j below +SIZE+
	       do (if (not(eq(aref grid i j)0))
		      (incf c))))
  (if (eq c (* +SIZE+ +SIZE+))
      T
      NIL)))


(defun sudoku-main (grid g)
  (afficher-sudoku grid)
  (play grid g)
  (if (win +grid+)
      (princ "vous avez gagné")
      (sudoku-main grid g)))

(defun sudoku(grid)
  (let ((g (grid-copy grid )))
    (sudoku-main grid g)))


