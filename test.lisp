
;;;;;;;;;;;;;;;;;;;;;;;;;Test.lisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Ce fichier contient les tests de certaine fonctions que nous
;;; avons jugé bon de tester.
;;;
;;; Une fonction main lance l'intégralité des tests et renvoie
;;; True si tous les tests sont réussis
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; teste si grid-copy fonctionne

(defun test-copy-grid(grid)
  (let((g (grid-copy grid)))
    (if (equalp g grid)
	T
	NIL)))


;;teste si test-valeur fonctionne en lui passant une grid et une grid contenant
;;toute la liste de possibilités et vérifie qu'elle peut la générer sans erreur
;;Pour que ce test fonctionne, les fonctions test-collone test-ligne et test-carre doivent également etre fonctionnelles
;;On considerera donc que ce test vérifie aussi ces fonctions.

(defun test-test-valeur (grid vrais-grid)
  (let ((g (make-array '(9 9):initial-element '(1 2 3 4 5 6 7 8 9))))
    (loop for i below +SIZE+
	  do (loop for j below +SIZE+
		   do (loop for v from 1 to +SIZE+
			    do (if (not(test-valeur grid j i v))
				   (setf (aref g i j)(remove v (aref g i j)))))))
    (equalp g vrais-grid)))




;;vérifie si une stratégie marche en lui donnant une grille particulière

(defun test-strat (init main grid)
  (let ((g (grid-copy grid)))
    (funcall init g)
    (labels ((intern-test-strat ()
	       (multiple-value-bind (i j v) (funcall main)
		 (if (not (null v))
		     (progn (setf (aref g i j) v)
			    (intern-test-strat))))))
      (intern-test-strat)
      (afficher-sudoku g)
      (win g))))



;;teste si init-standalone renvoie la bonne grille de possibilité de valeur de la grid passee en parametre
;;cette fonction obtenant son résultat grace aux fonctions supprimer-colonne supprimer-ligne supprimer-carre
;;nous considérerons qu'elles fonctionnent également

(defun test-init-standalone (grid-test vrais-grid)
  (let ((pgrid (init-standalone grid-test)))
    (if (equalp pgrid vrais-grid)
	T
	NIL)))




;;lance tous les tests

(defun main-test()
  (if (and(test-copy-grid +grid-test+)
	  (test-test-valeur +grid-test+ +possible-grid-test+)
	  (test-init-standalone +grid-test+ +possible-grid-test+)
	  (test-strat #'init-standalone #'main-standalone +grid+))
      T
      NIL))
