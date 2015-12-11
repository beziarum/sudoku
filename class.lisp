(load "affichage")
(load "strategies")
(load "strategy2")
(load "constante")
(load "test")


;;;;;;;;;;;;;;;;;;;;;;;;;Sudoku.lisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Ce fichier contient le principale code du jeu solo du programme.
;;;  
;;; Les compléxitées seront données en fonction de n pour n = +size+.
;;; +SIZE+ correspond a la longueur d'un coté de la grille
;;;
;;; Nous avons opté pour une implémentation principalement itérative jugant
;;; jugant l'abstraction offerte par l'objet relativement peu intérréssente 
;;; dans un projet de cette envergure.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Effectue une copie d'une grille de jeu
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
	(y (- c (mod c +CARRE-SIZE+))))        ;se qui nous permetra de le parcourir
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


;; fonction permettant de verifier que colonne ligne et valeur sont des données crédibles.
;;
;; permet une forte robustesse du main : le joueur ne fera pas cracher le jeux en tapant nimporte quoi
;; Complexité O(1)
(defun triplet-valide (c l v)
  (if (and(numberp c)
	  (numberp l)
	  (numberp v))
      (if(and
	  (and(<= c +SIZE+)(>= c 0))
	  (and(<= l +SIZE+)(>= l 0))
	  (and(<= v +SIZE+)(>= v 0)))
	 T
	 NIL)
      NIL))



;; Fonction permetant de transformer le caractere c en une valeur
;; correspondant a la colonne.
;; Complexité O(1)

(defun transformation (c)
  (labels ((tInter (c n l)
	   (cond ((eq (car l) c) n)
		 ((null (cdr l)) n)
		 (T (tInter c (1+ n) (cdr l))))))
    (tInter c 0 '(A B C D E F G H I))))


;; fonction qui gere le programme à chaque tour
;;
;; Voila comment se déroule le jeu:
;;
;; A chaque tour on rentre des coordonées dans la console et une valeur
;; Si la valeur est égale a 0 c'est qu'on souhaite effacer un coup précédent on va alors verifier qu'il
;; n'y avait pas de valeur sur la grille initiale et effacer sinon on renvoie un message
;;
;; Si val vaut une valeur de 0 a 9 alors on regarde si le coup respecte les regle et si c'est le cas on place la valeur
;; Complexité O(n)

(defun play (grid grid-copy)
  (let ((c (progn
	     (format t "Choix de valeur (valeur 0 = effacer)~%")
	     (format t "COL LIG VAL?~% ")
	     (read))) 
	(l  (read))
	(valeur  (read)))
    (setf c (transformation c))           ;transforme la lettre de la colone en chiffre 
					  ;(voir fichier lecture)
    (if(triplet-valide c l valeur)        ;permet d'éviter les erreurs en cas de mauvaise saisie
       (progn
	 (decf l)                         ; nous on vas de 0 a 8 pas de 1 a 9 (pour c la décrémentation se fait a la transformation
	 (if (zerop valeur)
	     (if (test-delete-valeur grid grid-copy c l)   ; cas ou on cherche a supprimer une valeur
		 (delete-valeur grid c l)
		 (progn
		   (format t "Impossible de supprimer cette valeur~% ")
		   (play grid grid-copy)))
	     (if (test-valeur grid c l valeur)                ;cas ou on cherche a jouer
		 (set-valeur grid c l valeur)
		 (progn
		   (format t"Impossible d'attribuer cette valeur à cette case ~% ")
		   (play grid grid-copy)))))
       (progn
	 (format t"Vous avez rentré des valeurs non valables ~%")
	 (play grid grid-copy)))))



;; vérifie si une grille est finie d'etre remplie
;;comme le joueur ne peut placer de coup non valable une grille remplie signifie une grille réussie!
;;Complexité O(n²)
(defun win (grid)
  (let ((c 0))
    (loop for i below +SIZE+
	  do (loop for j below +SIZE+
		   do (if (not(eq(aref grid i j)0))
			  (incf c))))
    (if (eq c +AREA+)
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
  (let ((g (grid-copy grid )); on fait deux copie la premiere est celle sur laquel on va jouer sa évite de modifier la grille principal
	(g2 (grid-copy grid)));la deuxieme permet de garder une copie de la grille initial pour savoir ou étais les valeur de départ
    (sudoku-main g g2)))




