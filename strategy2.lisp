

(defun supprimer-colone (grid e j)
  (loop for i below 9
     do (setf (aref grid i j) (remove (aref grid i j) e))))

(defun supprimer-ligne (grid e i)
  (loop for j below 9
     do (setf (aref grid i j) (remove (aref grid i j) e))))

(defun supprimer-carre (grid e x y)
  (let ((lcarrex (mod x +CARRE-SIZE+))
	(lcarrey (mod y +CARRE-SIZE+)))
    (loop for i from (- x lcarrex) to (- (+ x +CARRE-SIZE+) lcarrex)
       do (loop for j from (- x lcarrey) to (- (+ x +CARRE-SIZE+) lcarrey)
	       do (setf (aref grid i j) (remove (aref grid i j) e))))))
