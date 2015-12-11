(load "affichage")
(load "class.lisp")

;;; 1ere strategie (aléatoire)
;;; la stratégie joue un coup aléatoire parmi les coups autorisés


;; Fonction qui compte le nombre de cases vides dans un sudoku

(defun number-of-zeroes (grid)
  (let ((cpt 0))
    (loop for i below +SIZE+
       do (loop for j below +SIZE+
	     do (if (eq (aref grid i j) 0)       ; si la case est égale à 0 (donc vide), on incrémente le compteur
		    (incf cpt))))
    cpt))


;; fonction qui renvoie la position du n-ième zero du sudoku

(defun position-zero (grid n)
  (let ((tmp n)                                       ; n (donc tmp) représente le n-ième zéro dans la grille
	(k 0)                                         ; k et l sont les indices à renvoyer
	(l 0))
    (loop for i below +SIZE+                         
       do (loop for j below +SIZE+ never (eq tmp 0)   ; on parcourt la grille tant que tmp n'est pas égal à 0
	     do (if (eq (aref grid i j) 0)            ; quand on rencontre un zéro, on decrémente tmp
		    (decf tmp))
	     do (when (eq tmp 0)                      ; on sauvergarde les indices
		  (setf k i)
		  (setf l j)
		  )))
    (values k l)))
	      

;; fonction qui renvoie une liste de probabilité selon une case

(defun possibility-list (grid colonne ligne)
  (let ((l '()))
    (loop for i below +SIZE+
       do (if (test-valeur grid colonne ligne (+ 1 i))      ; si la valeur est possible
	      (push (+ 1 i) l)))                            ; on la stock dans l
    l))


;; fonction qui effectue la stratégie sur la grille

;;;;;;;;;;;;;;warning a corriger tu met grid zn param mais tu l'utilise pas;;;;;;;;

(defun random-strat (grid)
  (let ((place (random (number-of-zeroes grid))))                   ; place représente un zéro aléatoire
    (incf place)                                                    ; 
    (multiple-value-bind (j i) (position-zero grid place)           ; on récupère la position du zéro 
      (let* ((l (possibility-list grid i j)))             
	(if (eq l NIL)                                              
	    NIL                                                     ; si la liste de probabilité de la case est nulle, on renvoie NIL
	    (set-valeur grid i j (nth (random (length l)) l)))))))  ; sinon on lui assigne un élément aléatoire de la liste de possibilité
  


  
