

;;;;;;;;;;;;;;;;;;;;;;;;;Sudoku.lisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Ce fichier contient le code de la partie solo du programme.
;;;  
;;; Les compléxités seront données en fonction de n pour n = +size+.
;;; +SIZE+ correspond a la longueur d'un coté de la grille
;;;
;;; Nous avons opté pour une implémentation principalement itérative jugeant
;;; l'abstraction offerte par l'objet relativement peu intérréssante 
;;; dans un projet de cette envergure.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(load "affichage")
(load "constante")
(load "utilitaire")


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



;; fonction qui gere le programme à chaque tour
;;
;; Voila comment se déroule le jeu:
;;
;; A chaque tour on rentre des coordonées dans la console et une valeur
;; Si la valeur est égale a 0 c'est qu'on souhaite effacer un coup précédent on va alors verifier qu'il
;; n'y avait pas de valeur sur la grille initiale et effacer sinon on renvoie un message
;;
;; Si val vaut une valeur de 0 a 9 alors on regarde si le coup respecte les regles et si c'est le cas on place la valeur
;; Complexité O(n)

(defun play (grid grid-copy)
  (let ((c (progn
	     (format t "Choix de valeur (valeur 0 = effacer)~%")
	     (format t "COL LIG VAL?~% ")
	     (read))) 
	(l  (read))
	(valeur  (read)))
    (setf c (transformation c))           ;transforme la lettre de la colonne en chiffre 
					  ;(voir fichier lecture)
    (if(triplet-valide c l valeur)        ;permet d'éviter les erreurs en cas de mauvaise saisie
       (progn
	 (decf l)                         ; nous on va de 0 a 8 pas de 1 a 9 (pour c la décrémentation se fait a la transformation
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


;; fonction main

(defun sudoku-main (grid g)
  (afficher-sudoku grid)
  (play grid g)
  (if (win +grid+)
      (format t "vous avez gagné ~%")
      (sudoku-main grid g)))

;;fonction qui fait la copie de la grid et lance le main

(defun sudoku(grid)
  (let ((g (grid-copy grid )); on fait deux copies la premiere est celle sur laquelle on va jouer sa évite de modifier la grille principale
	(g2 (grid-copy grid)));la deuxieme permet de garder une copie de la grille initiale pour savoir ou étaient les valeur de départ
    (sudoku-main g g2)))




