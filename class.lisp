
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

;;effectue une copie d'une grille de jeux

(defun grid-copy (grid)
  (let ((g (make-array '(9 9) :element-type (array-element-type grid))))
    (loop for i below 9
       do (loop for j below 9
	     do (setf (aref g i j) (aref grid i j))))
    g))

;;test si la valeur est deja presente dans la colone

(defun test-colonne(grid c valeur)
  (loop for i below +size+
      do (if (eq(aref grid i c)valeur)
	     NIL
	     T)))

;;test si la valeur est deja presente dans la ligne

(defun test-ligne(grid l valeur)
  (loop for i below +size+ never (eq (aref grid l i) valeur)
      finally (return T)))

;;test si la valeur est deja presente dans le carré

(defun test-carre(grid c l valeur)
  (let ((x (* (floor (/ c +CARRE-SIZE+)) +CARRE-SIZE+))
       (y (* (floor (/ l +CARRE-SIZE+)) +CARRE-SIZE+)))
    (loop for i from x to (+ x +CARRE-SIZE+)
       do (loop for j from y to (+ y +CARRE-SIZE+)
	     do (if (eq(aref grid x y)valeur)
		    NIL
		    T)))))
