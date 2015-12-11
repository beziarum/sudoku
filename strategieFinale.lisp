;;;;;;;;;;;;;;;;;;;;;;;;;strategieFinale.lisp;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Ce fichier contient les fonctions de notre stratégie finale.
;;; Cette stratégie résout toutes les grilles proposées par le
;;; programme de test du cremi mais pas toutes les grilles existantes
;;; La stratégie de backtracking n'étant pas utilisé, les coups sont
;;; trouvés assez rapidement
;;;
;;; Les différentes complexités sont indiquées en fonction du côté de
;;; la grille, c'est à dire 9 dans le cas d'une grille standart.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +carre-size+ 3)

;;prend en parametre une grille de possibilité, une valeur et un
;;numéro de colonne, supprime toutes les occurences de e dans les
;;listes de probabilités des cases situées sur la colonne j de la grille
;;O(n²)
(defun supprimer-colone (grid e j)
  (loop for i below 9
     do (setf (aref grid i j) (remove e (aref grid i j))))) ;O(n)


;;prend en parametre une grille de possibilité, une valeur et un
;;numéro de ligne, supprime toutes les occurences de e dans les
;;listes de probabilités des cases situées sur la ligne i de la grille
;;O(n²)
(defun supprimer-ligne (grid e i)
  (loop for j below 9
     do (setf (aref grid i j) (remove e (aref grid i j))))) ;O(n)


;;prend en parametre une grille de possibilité, une valeur et une
;;position de case, supprime toutes les occurences de e dans les
;;listes de probabilité des cases situés sur le même carré que la
;;case donné en parametre
;;O(n²)
(defun supprimer-carre (grid e x y)
  (let ((lcarrex (mod x +CARRE-SIZE+))
	(lcarrey (mod y +CARRE-SIZE+)))
    (loop for i from (- x lcarrex) below (- (+ x +CARRE-SIZE+) lcarrex) ;on a √n tours de boucles
       do (loop for j from (- y lcarrey) below (- (+ y +CARRE-SIZE+) lcarrey) ;idem
	     do (setf (aref grid i j) (remove e (aref grid i j))))))) ;O(n)

;;Prend en parametre une grille de probabilité et une position de case, cherche
;;si une seule des valeurs de la liste de possibilité de la case (i ; j) n'est
;;pas située dans les listes de possibilité des autres cases situés dans la même
;;colonne, si oui renvoie cette valeur, et nil sinon
;;O(n³)
(defun inclusive-colone (grid i j)
  (let ((possible (aref grid i j)))
    (loop for y below 9
       do (if (/= j y)
	      (setf possible (set-difference possible (aref grid i y)))));O(n²)
    (if (null (cdr possible))
	(car possible)
	nil)))

;;Prend en parametre une grille de probabilité et une position de case, cherche
;;si une seule des valeurs de la liste de possibilité de la case (i ; j) n'est
;;pas située dans les listes de possibilité des autres cases situés dans la même
;;ligne, si oui renvoie cette valeur, et nil sinon
;;O(n³)
(defun inclusive-ligne (grid i j)
  (let ((possible (aref grid i j)))
    (loop for x below 9
       do (if (/= x i)
	      (setf possible (set-difference possible (aref grid x j)))));O(n²)
    (if (null (cdr possible))
	(car possible)
	nil)))


;;Prend en parametre une grille de probabilité et une position de case, cherche
;;si une seule des valeurs de la liste de possibilité de la case (i ; j) n'est
;;pas située dans les listes de possibilité des autres cases situés dans le même
;;carré, si oui renvoie cette valeur, et nil sinon
;;O(n³)
(defun inclusive-carre (grid i j)
  (let ((possible (aref grid i j))
	(xcarre (- i (mod i +CARRE-SIZE+)))
	(ycarre (- j (mod j +CARRE-SIZE+))))
    (Loop for x from xcarre below (+ xcarre +CARRE-SIZE+) ;on a √n tours de boucles
       do (loop for y from ycarre below (+ ycarre +CARRE-SIZE+) ;idem
	     if (or (/= j y) (/= i x))
	     do (setf possible (set-difference possible
					       (aref grid x y)));O(n²)
    (if (null (cdr possible))
	(car possible)
	nil)))))

;;prend en parametre une grille de possibilités et essaie d'y trouver
;;un coup jouable en utilisant la stratégie exlusive : si une des listes
;;de possibilité ne contient qu'un seul élément alors on est sûr que cet
;;élément est situé sur la case correspondante, on met à jour alors les
;;liste de possibilités des cases situés dans la même ligne, colonne et
;;dans le même carre, puis renvoie la valeur précédemment trouvé ainsi que
;;sa position.
;;O(n²)
(defun exclusive-strat (grid)
    (let ((ret nil))
      (loop
	 for i below 9 always (null ret)
	 do (loop
	       for j below 9 always (null ret)
	       do (if (and (null (cdr (aref grid i j)))        ;on exécute les fonctions qui suivent 
			   (not (null (car (aref grid i j))))) ;une seule fois dans la fonction
		      (progn (setf ret (list i
					     j
					     (car (aref grid i j))))
			     (supprimer-colone grid (caddr ret) j) ;O(n²)
			     (supprimer-ligne grid (caddr ret) i) ;O(n²)
			     (supprimer-carre grid (caddr ret) i j) ;O(n²)
			     (setf (aref grid i j) nil)))))
      (values-list ret)))

;;Prend en parametre une grille de possibilités et un numéro de colonne et la fonction
;;simplifie cette grille pour cette colonne.
;;Elle renvoi T si une simplification a pu être faite, nil sinon
;;O(n⁵)
(defun simplifier-colonne (grid i) 
  (let ((simp nil))
    (loop for y1 below 9
       do (loop for y2 from (1+ y1) below 9
	     if (and (equal (aref grid i y1) (aref grid i y2))
		     (= (length (aref grid i y1)) 2))
	     do (loop for y3 below 9
		   if (and (/= y1 y2 y3)
			   (intersection (aref grid i y3)
					 (aref grid i y2))) ;O(n²)
		   do (progn (setf (aref grid i y3)
				   (set-difference (aref grid i y3)
						   (aref grid i y2))) ;O(n²)
			     (setf simp t)))))
    simp))

;;Prend en parametre une grille de possibilités et un numéro de colonne et la fonction
;;simplifie cette grille pour cette colone.
;;Elle renvoi T si une simplification a pu être faite, nil sinon
;;O(n⁵)
(defun simplifier-ligne (grid j)
  (let ((simp nil))
    (loop for x1 below 9
       do (loop for x2 from (1+ x1) below 9
	     if (and (equal (aref grid x1 j) (aref grid x2 j))
		     (= (length (aref grid x1 j)) 2))
	     do (loop for x3 below 9
		   if (and (/= x1 x2 x3)
			   (intersection (aref grid x3 j)
					 (aref grid x2 j))) ;O(n²)
		   do (progn (setf (aref grid x3 j)
				   (set-difference (aref grid x3 j)
						   (aref grid x2 j))) ;O(n²)
			     (setf simp t)))))
    simp))

;;Prend en parametre une grille de possibilités et une position de case située en
;;haut à gauche d'un carre. La fonction tente alors de simplifier ce carre
;;et renvoi T si elle y est arrivé, nil sinon
;;O(n⁵)
(defun simplifier-carre (grid i j)
  (let ((simp nil))
    (loop for x1 from i below (+ 3 i) ;on a √n tours de boucles
       do (loop for y1 from j below (+ 3 j) ;idem
	     do (loop for x2 from x1 below (+ 3 i) ;on a √n tours de boucles
		   do (loop for y2 from (if (= x1 x2) (+ 1 y1) j) below (+ 3 j) ;au maximum là aussi
			 if (and (equal (aref grid x1 y1) (aref grid x2 y2))
				 (= (length (aref grid x1 y1)) 2))
			 do (loop for x3 from i below (+ 3 i) ;on a √n tours de boucles
			       do (loop for y3 from j below (+ 3 j) ;idem
				     if (and (and (or (/= x1 x3)
						      (/= y1 y3))
						  (or (/= x2 x3)
						      (/= y2 y3)))
					     (intersection (aref grid x3 y3)
							   (aref grid x2 y2))) ;O(n²)
				     do (progn (setf (aref grid x3 y3)
						     (set-difference (aref grid x3 y3)
								     (aref grid x2 y2))) ;O(n²)
					       (setf simp t))))))))
    simp))

;;Cette fonction essai de simplifier une grille de possibilité
;;pour celà elle va simplifier chaque ligne, colonne et carre.
;;Elle renvoi T si une simplification a pu être faite, nil sinon.
;;Pour celà elle utilise la technique des paires exclusives
;;Si deux cases dans une même zone (c'est à dire une colonne, une
;;ligne ou un carre) possèdent la même paire de valeur, alors
;;toutes les autres cases de cette zone ne peuvent pas être de
;;l'une de ces deux valeurs, on met alors à jours les listes
;;de possibilités
;;O(√n×n⁶)
(defun simplifier-sudoku (grid)
  (let ((simp nil))
    (loop for n below 9
       do (progn (setf simp (or (simplifier-colonne grid n) simp)) ;O(n⁵) 
		 (setf simp (or (simplifier-ligne grid n) simp)) ;O(n⁵)
  		 (if (= (mod n 3) 0)
  		     (loop for m below 9 by 3
  		 	do (setf simp (or (simplifier-carre grid n m) simp)))))) ;O(n⁵)
    simp))


;;Cette fonction prend en parametre une grille de possibilités et
;;essai d'y trouver un coup valide en utilisant la stratégie inclusive :
;;si une valeur est présente dans une seule des listes de probabilités d'une
;;même colone (ou grille, ou carre) alors on sait que cette valeur est situé
;;à cet position. La fonction met alors à jour les listes de possibilité des
;;cases situés sur la même ligne, colonne et carre, puis renvoi la valeur
;;précédement trouvé ainsi que sa position.
;;O(n³)
(defun inclusive-strat (grid)
    (let ((ret '(0 0 nil)))
      (loop for i below 9 always (null (caddr ret))       ;les fonction qui suivent ne sont exécutés
	 do (loop for j below 9 always (null (caddr ret)) ;qu'une seule fois dans la fonction
	       do (progn (setf ret (list i
					 j
					 (inclusive-colone grid i j))) ;O(n³)
			 (if (null (caddr ret))
			     
			       (setf ret (list i
					     j
					     (inclusive-ligne grid i j)))) ;O(n³)
			 (if (null (caddr ret))
				   
			     (setf ret (list i
					     j
					     (inclusive-carre grid i j))))))) ;O(n³)
      (if (null (caddr ret))
	  nil
	  (progn (supprimer-colone grid (caddr ret) (cadr ret)) ;O(n²)
		 (supprimer-ligne grid (caddr ret) (car ret)) ;O(n²)
		 (supprimer-carre grid (caddr ret) (car ret) (cadr ret)) ;O(n²)
		 (setf (aref grid (car ret) (cadr ret)) nil)
		 (values-list ret)))))


;;Les fonction suivantes utilisent toutes une grille de probabilité commune
(let ((pgrid nil))
  ;;Initialise la grille de probabilité en suivant une grille donné en parametre
  ;;O(n⁴)
  (defun init-standalone (grid)
    (setf pgrid (make-array '(9 9):initial-element '(1 2 3 4 5 6 7 8 9)))
    (loop for i below 9
       do (loop for j below 9
	     do (if (/= (aref grid i j) 0)
		    (progn
		      (supprimer-colone pgrid (aref grid i j) j) ;O(n²)
		      (supprimer-ligne pgrid (aref grid i j) i) ;O(n²)
		      (supprimer-carre pgrid (aref grid i j) i j) ;(n²)
		      (setf (aref pgrid i j) nil)))))
    pgrid)

  ;;Cette fonction vérifie si l'une des stratégies implémentés peut trouver un
  ;;coup valide par rapport à la grille de probabilité interne (dans l'ordre :
  ;;d'abord la stratégie exlusive, puis la stratégie inclusive si la première
  ;;n'as pas trouvé de valeur possible), si aucun coups n'as été trouvé alors
  ;;on va essayer de simplifier la grille et si on y arrive alors on relance
  ;;la fonction depuis le début
  ;;Si un coup possible a été trouvé alors il est renvoyé
  (defun main-standalone ()
    (let ((l (multiple-value-list (exclusive-strat pgrid)))) ;O(n²)
      (if (null (car l))
	  (setf l (multiple-value-list (inclusive-strat pgrid)))) ;O(n³)
      (if (and (null (car l))
	       (simplifier-sudoku pgrid)) ;O(√n×n⁶)
	  (main-standalone)
	  (values-list l)))))


