(load "affichage")
(load "class.lisp")

;;; 1ere strategie (stupide)


(defun number-of-zeroes (grid)
  (let ((cpt 0))
    (loop for i below 9
       do (loop for j below 9
	     do (if (eq (aref grid i j) 0)
		    (incf cpt))))
    cpt))


(defun position-zero (grid value)
  (let ((tmp value)
	(k 0)
	(l 0))
    (loop for i below 9
       do (loop for j below 9 never (eq tmp 0)
	     do (if (eq (aref grid i j) 0) 
		    (decf tmp))
	     do (when (eq tmp 0)
		  (setf k i)
		  (setf l j)
		  )))
    (values k l)))
	      
		   
(defun possibility-list (grid colonne ligne)
  (let ((l '()))
    (loop for i below 9
       do (if (test-valeur grid colonne ligne (+ 1 i))
	      (push (+ 1 i) l)))
    l))


(defun random-strat (grid)
  (let ((place (random (number-of-zeroes grid))))
    (incf place)
    (print place)
    (multiple-value-bind (j i) (position-zero grid place) ; i = colonne
      (let* ((l (possibility-list grid i j)))             ; l = liste des possibilités
	(if (eq l NIL)
	    NIL
	    (set-valeur grid i j (nth (random (length l)) l))))))) ; element aleatoire parmi l
  


;(defun test-position ()
 ; (loop for i below (number-of-zeroes +grid+)
;	do (print (position-zero +grid+ (+ i 1)))))
    
(defun remain-list (l)
  (let ((l2 '()))
    (loop for i below 9
	  do (if (not (member (+ 1 i) l))
		 (push (+ 1 i) l2)))
    l2))
    
(defun is-not-possible (grid colonne ligne)
    (let ((l '()))
    (loop for i below 9
       do (if (not (test-valeur grid colonne ligne (+ 1 i)))
	      (push (+ 1 i) l)))
    l))
    

      
      
  
