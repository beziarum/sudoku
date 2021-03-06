

;;;;;;;;;;;;;;;;;;;;;;;;;RandomStrat.lisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ce fichier contient la 1ere strategie (aléatoire)
;;; La stratégie joue un coup aléatoire parmi les coups autorisés
;;; La stratégie est effectuée par la fonction test-random-strat
;;; qui teste également la stratégie
;;;
;;; Il n'y a pas de fonction init-standalone et main-standalone 
;;; car nous avons cru comprendre que ces modalités concernent uniquement 
;;; la stratégie utilisée pour le tournoi
;;;
;;; Cette stratégie n'est pas implémentée dans un fichier indépendant
;;; Elle ne pourra fonctionner sans "utilitaire.lisp"
;;; Mais elle n'a pas pour but d'être utilisée pour le tournoi
;;;
;;; Les complexités sont exprimées en fonction de +SIZE+ 
;;;
;;; Cette stratégie a une chance infiniment faible de résoudre un sudoku
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "utilitaire")

;; Fonction qui compte le nombre de cases vides dans un sudoku
;; complexité : O(n²)

(defun number-of-zeroes (grid)
  (let ((cpt 0))
    (loop for i below +SIZE+
       do (loop for j below +SIZE+
	     do (if (eq (aref grid i j) 0)       ; si la case est égale à 0 (donc vide), on incrémente le compteur
		    (incf cpt))))
    cpt))


;; fonction qui renvoie la position du n-ième zero du sudoku
;; complexité : O(n²)

(defun position-zero (grid n)
  (assert (<= n (number-of-zeroes grid)))
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
;; complexité : O(n²)

(defun possibility-list (grid colonne ligne)
  (assert (and (>= colonne 0) (< colonne +SIZE+)))
  (assert (and (>= colonne 0) (< colonne +SIZE+)))
  (let ((l '()))
    (loop for i below +SIZE+
       do (if (test-valeur grid colonne ligne (+ 1 i))      ; si la valeur est possible
	      (push (+ 1 i) l)))                            ; on la stock dans l
    l))


;; fonction qui détermine s'il existe un coup possible 
;; complexité : O(n³)

(defun is-possible (grid)
  (let ((bool NIL))
    (loop for i below 9
	  do (loop for j below 9
		   do (if (not (equalp (possibility-list grid i j) '()))   ; si la liste de possibilité d'une case est non nulle
			  (setf bool T))))                                 ; on passe le booléen à True
    bool))


;; fonction qui renvoie les coordonées de la case dans laquelle 
;; on veut jouer et la valeur à inscrire dedans
;; la fonction renvoie NIL si il n'y a plus de coup à jouer
;; complexité : O(n³)

(defun random-strat (grid)
  (if (is-possible grid)                                                ; tant qu'on peut jouer
      (let ((place (random (number-of-zeroes grid))))                   ; place représente un zéro aléatoire
	(incf place)                                                    ; on incrémente car il y a un décalage (on veut 0 < place <= number-of-zeroes)
	(multiple-value-bind (j i) (position-zero grid place)           ; on récupère la position du zéro 
	  (let* ((l (possibility-list grid i j)))             
	    (if (eq l NIL)                                              
		(random-strat grid)                                     ; si la liste de probabilité de la case est nulle, on rappelle la stratégie
		(values i j (nth (random (length l)) l))))))            ; sinon on renvoie les coordonées de la case à modifier, et la valeur à lui assigner
      (progn (print "plus de valeur possible")
	     NIL)))     


;; fonction qui exécute et teste la stratégie aléatoire
;; complexité : O(n⁴)

(defun test-random-strat ()
  (let((g (grid-copy +grid+)))
  (loop while (not (eq (random-strat g) NIL))                          ; tant qu'il reste des possibilités
	do (multiple-value-bind (i j k) (random-strat g)
	     (set-valeur g i j k)))                                ; on joue
  (if (is-possible g)                                              ; on vérifie qu'il ne reste effectivement pas de possibilité
      NIL
      T)))
	     
