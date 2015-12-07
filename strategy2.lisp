

(defun supprimer-colone (grid e j)
  (loop for i below 9
     do (setf (aref grid i j) (remove e (aref grid i j)))))

(defun supprimer-ligne (grid e i)
  (loop for j below 9
     do (setf (aref grid i j) (remove e (aref grid i j)))))

(defun supprimer-carre (grid e x y)
  (let ((lcarrex (mod x +CARRE-SIZE+))
	(lcarrey (mod y +CARRE-SIZE+)))
    (loop for i from (- x lcarrex) below (- (+ x +CARRE-SIZE+) lcarrex)
       do (loop for j from (- y lcarrey) below (- (+ y +CARRE-SIZE+) lcarrey)
	     do (setf (aref grid i j) (remove e (aref grid i j)))))))


(let ((pgrid nil))
  (defun initialise-tab-strat (grid)
    (setf pgrid (make-array '(9 9):initial-element '(1 2 3 4 5 6 7 8 9)))
    (loop for i below 9
       do (loop for j below 9
	     do (if (/= (aref grid i j) 0)
		    (progn
		      (supprimer-colone pgrid (aref grid i j) j)
		      (supprimer-ligne pgrid (aref grid i j) i)
		      (supprimer-carre pgrid (aref grid i j) i j)
		      (setf (aref pgrid i j) nil)))))
    (print pgrid))


  (defun main-standalone ()
    (let ((ret nil))
      (loop
	 for i below 9 always (null ret)
	 do (loop
	       for j below 9 always (null ret)
	       do (if (and (null (cdr (aref pgrid i j)))
			   (not (null (car (aref pgrid i j)))))
		      (progn (setf ret (list j
					     i
					    (car (aref pgrid i j))))
			     (print i)
			     (print j)
			     (print (aref pgrid i j))
			     (supprimer-colone pgrid ret j)
			     (supprimer-ligne pgrid ret i)
			     (supprimer-carre pgrid ret i j)
			     (setf (aref pgrid i j) nil)))))
      (values (car ret) (cadr ret) (caddr ret)))))


(defun test-strat (init main grid)
  (funcall init grid)
  (labels ((intern-test-strat ()
	     (multiple-value-bind (i j v) (funcall main)
	       (if (not (null v))
		   (progn (print (aref grid j i))
			  (setf (aref grid j i) v)
			  (print (aref grid j i))
			  (afficher-sudoku grid)
			  (intern-test-strat))))))
    (intern-test-strat)))
