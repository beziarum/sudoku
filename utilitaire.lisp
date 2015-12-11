





;;;;;;;;;;;;;;;;;;;;;;;;;Utilitaire.lisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Ce fichier contient le code de certaine fonction utilisé par sudoku.lisp
;;;  
;;; Les compléxités seront données en fonction de n pour n = +size+.
;;; +SIZE+ correspond a la longueur d'un coté de la grille
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "constante")

;;Effectue une copie d'une grille de jeu
;; Complexité O(n²)

(defun grid-copy (grid)
  (let ((g (make-array '(9 9))))
    (loop for i below +SIZE+
	  do (loop for j below +SIZE+
		   do (setf (aref g i j)(aref grid i j))))
    g))

;; teste si valeur est deja presente dans la colonne
;; Complexité O(n)

(defun test-colonne(grid c valeur)
  (loop for i below +SIZE+ never (eq(aref grid i c)valeur)
	finally (return T)))

;; teste si valeur est deja presente dans la ligne
;; Complexité O(n)

(defun test-ligne(grid l valeur)
  (loop for i below +SIZE+ never (eq (aref grid l i) valeur)
	finally (return T)))

;; teste si valeur est deja presente dans le carré
;; Complexité O(n)
(defun test-carre(grid c l valeur)
  (let ((x (- l (mod l +CARRE-SIZE+)))         ; retourne le coin supérieur du carré
	(y (- c (mod c +CARRE-SIZE+))))        ; ce qui nous permetra de le parcourir
    (loop for i from x below (+ x +CARRE-SIZE+)
	  always (loop for j from y below (+ y +CARRE-SIZE+)
		       never (eq(aref grid i j)valeur)
		       finally(return T)))))


;; vérifie si une valeur est permise par les regles du jeu dans la grid
;; et aux coordonnées passé en paramètre
;; Complexité O(n)

(defun test-valeur (grid c l valeur)
  (if (and (test-ligne grid l valeur)
	   (test-colonne grid c valeur)
	   (test-carre grid c l valeur)
	   (eq(aref grid l c) 0)
	   (and(< valeur 10)(> valeur 0)))
      T
      NIL))

;;attribue une valeur à la grille

(defun set-valeur(grid c l valeur)
  (setf(aref grid l c)valeur))


;;vérifie si on peut retirer la valeur (faux si valeur est présente dans la grille de base)
;;Complexité O(1)
(defun test-delete-valeur(grid grid-copy c l)
  (if (and
       (zerop(aref grid-copy l c))
       (not (zerop (aref grid l c))))
      T
      NIL))

;; réatribue 0 a la case de coordonné c l

(defun delete-valeur (grid c l)
  (setf(aref grid l c)0))



;; Fonction permetant de transformer le caractere c en une valeur
;; correspondant a la colonne.
;; Complexité O(1)

(defun transformation (c)
  (labels ((tInter (c n l)
	   (cond ((eq (car l) c) n)
		 ((null (cdr l)) n)
		 (T (tInter c (1+ n) (cdr l))))))
    (tInter c 0 '(A B C D E F G H I))))
