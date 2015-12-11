;; test si grid-copy fonctionne

(defun test-copy-grid(grid)
  (let((g (grid-copy grid)))
    (if (equalp g grid)
	T
	NIL)))
;       (e 0))
;   (loop for i below +SIZE+
;	  do (loop for j below +SIZE+
;		  do (if (/=(aref grid i j)(aref g i j))
;			 (setf e 1))))
;    (if (eq e 1)
;	NIL
;	T)))

;;test si test-valeur fonctionne en lui passant une grid et une grid contenant
;;toute la liste de possibilité et vérifie qu'elle peut la générer sans erreur
;;Pour que ce test fonctionne les fonction test-collone test-ligne et test-care doive également etre fonctionnel
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
  (funcall init grid)
  (labels ((intern-test-strat ()
	     (multiple-value-bind (i j v) (time (funcall main))
	       (if (not (null v))
		   (progn (setf (aref grid j i) v)
			  (afficher-sudoku grid)
			  (intern-test-strat))))))
    (intern-test-strat)
    (afficher-sudoku grid)))

(defun main-test()
  (if (and(test-copy-grid +grid-test+)
	  (test-test-valeur +grid-test+ +possible-grid-test+))
      T
      NIL))
