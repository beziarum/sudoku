;;; 1ere strategie (stupide)

;; taille initial des grille de jeux
(defparameter +SIZE+ 9)
(defparameter +AREA+ (* +SIZE+ +SIZE+))
;(defparameter +CARRE-SIZE+)

;;grille initial du jeux

(defparameter +grid+ 
  (make-array '(9 9) :initial-contents
	      '((0 0 0 0 0 4 0 0 5)
		(0 0 0 9 5 0 0 8 0)
		(0 0 0 0 0 3 0 9 0)
		(0 0 5 0 0 2 0 0 4)
		(0 0 1 0 6 0 7 0 0)
		(7 0 0 3 0 0 2 0 0)
		(0 6 0 5 0 0 0 0 0)
		(0 8 0 0 1 6 0 0 0)
		(5 0 0 2 0 0 0 0 7))))

(defun grid-copy (grid)
  (let ((g (make-array '(9 9) :element-type (array-element-type grid))))
    (loop for i below 9
       do (loop for j below 9
	     do (setf (aref g i j) (aref grid i j))))
    g))


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
       do (loop for j below 9
	     do (if (eq (aref grid i j) 0)
		    (decf tmp))
	     do (when (eq tmp 0)
		  (setf k i)
		  (setf l j)
		  (return)
		  )))	 
    (values k l)))
		      
		   
		       
	       
	     

;(defun test-case (grid colonne ligne) a voir selon les fontions de borde.
 

;(defun random-strat (grid)
 ; (let ((place (random (number-of-zeroes +grid+)))
;	(value (random 9)))
 ;   (+ value 1) ;sale
    
    
    
    
    
    
  
