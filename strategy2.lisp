

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

(defun inclusive-colone (grid i j)
  (let ((possible (aref grid i j)))
    (loop for y below 9
       do (if (/= j y)
	      (setf possible (set-difference possible (aref grid i y)))))
    ;(print possible)
    (if (null (cdr possible))
	(car possible)
	nil)))

(defun inclusive-ligne (grid i j)
  (let ((possible (aref grid i j)))
    (loop for x below 9
       do (if (/= x i)
	      (setf possible (set-difference possible (aref grid x j)))))
    (if (null (cdr possible))
	(car possible)
	nil)))
(defun inclusive-carre (grid i j)
  (let ((possible (aref grid i j)))
    (loop for x below 9
       if (/= i x)
       do (loop for y below 9
	     if (/= j y)
	     do (setf possible (set-difference possible
					       (aref grid x y)))))
    (if (null (cdr possible))
	possible
	nil)))

(defun exclusive-strat (grid)
    (let ((ret nil))
      (loop
	 for i below 9 always (null ret)
	 do (loop
	       for j below 9 always (null ret)
	       do (if (and (null (cdr (aref grid i j)))
			   (not (null (car (aref grid i j)))))
		      (progn (setf ret (list j
					     i
					    (car (aref grid i j))))
			     (supprimer-colone grid ret j)
			     (supprimer-ligne grid ret i)
			     (supprimer-carre grid ret i j)
			     (setf (aref grid i j) nil)))))
      (values-list ret)))

(defun inclusive-strat (grid)
    (let ((ret nil))
      (loop for i below 9 always (null ret)
	 do (loop for j below 9 always (null ret)
	       do (progn (setf ret
			       (multiple-value-list (inclusive-colone grid i j))))
		 (if (null ret)
		     (setf ret
			   (multiple-value-list (inclusive-ligne grid i j))))
		 (if (null ret)
		     (setf ret
			   (multiple-value-list (inclusive-carre grid i j))))))
      (values-list ret)))

(let ((pgrid nil)
      (travail-grid nil))
  (defun initialise-tab-strat (grid)
    (setf travail-grid grid)
    (setf pgrid (make-array '(9 9):initial-element '(1 2 3 4 5 6 7 8 9)))
    (loop for i below 9
       do (loop for j below 9
	     do (if (/= (aref grid i j) 0)
		    (progn
		      (supprimer-colone pgrid (aref grid i j) j)
		      (supprimer-ligne pgrid (aref grid i j) i)
		      (supprimer-carre pgrid (aref grid i j) i j)
		      (setf (aref pgrid i j) nil))))))

  (defun test ()
    (inclusive-strat pgrid))
  (defun main-standalone ()
    (multiple-value-bind (i j v) (exclusive-strat pgrid)
      (if (not (null v))
	  (progn (print pgrid) (values i j v))
	  (inclusive-strat pgrid)))))

(defun test-strat (init main grid)
  (funcall init grid)
  (labels ((intern-test-strat ()
	     (multiple-value-bind (i j v) (time (funcall main))
	       (if (not (null v))
		   (progn (setf (aref grid j i) v)
			  (afficher-sudoku grid)
			  (intern-test-strat))))))
    (intern-test-strat)))
