;; test si grid-copy fonctionne

(defun test-copy-grid(grid)
  (let((g (grid-copy grid))
       (e 0))
    (loop for i below +SIZE+
	  do (loop for j below +SIZE+
		  do (if (/=(aref grid i j)(aref g i j))
			 (setf e 1))))
    (if (eq e 1)
	NIL
	T)))

;;test si test-valeur fonctionne en lui passant une grid et une grid contenant 
;;toute la liste de possibilité et vérifie qu'elle peut la générer sans erreur

(defun initialise-tab-strat (grid vrais-grid)
  (let ((g (make-array '(9 9):initial-element '(NIL)))
    (loop for i below 9
       do (loop for j below 9
	       do (loop for v from 1 to 9
		     do (if (test-valeur grid i j v
			      (setf (aref g i j) ))))))
