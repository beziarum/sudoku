;;; 1ere strategie (stupide)

;; taille initial des grille de jeux
(defparameter +SIZE+ 9)
(defparameter +AREA+ (* +SIZE+ +SIZE+))
;(defparameter +CARRE-SIZE+)

;;grille initial du jeux

(defparameter +grid+ 
  (make-array '(9 9) :initial-contents
	      '((1 0 0 0 0 4 0 0 5)
		(0 0 0 9 5 0 0 8 0)
		(0 0 0 0 0 3 0 9 0)
		(0 0 5 0 0 2 0 0 4)
		(0 0 1 0 6 0 7 0 0)
		(7 0 0 3 0 0 2 0 0)
		(0 6 0 5 0 0 0 0 0)
		(0 8 0 0 1 6 0 0 0)
		(5 0 0 2 0 0 0 0 7))))

(defun number-of-zeroes (grid)
  (let ((cpt 0))
    (loop for i below 9
       do (loop for j below 9
	     do (if (eq (aref grid i j) 0)
		    (incf cpt))))
    cpt))

;(defun random-strat (grid c)
  
