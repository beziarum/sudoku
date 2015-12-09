(defparameter +carre-size+ 3)

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
    ;(print 'flood3)
    (loop for y below 9
       do (if (/= j y)
  	      (setf possible (set-difference possible (aref grid i y)))))
    ;(print possible)
    (if (null (cdr possible))
  	(car possible)
  	nil)))

(defun inclusive-ligne (grid i j)
(let ((possible (aref grid i j)))
    ;(print 'flood2)
    (loop for x below 9
       do (if (/= x i)
  	      (setf possible (set-difference possible (aref grid x j)))))
    (if (null (cdr possible))
  	(car possible)
  	nil)))

(defun inclusive-carre (grid i j)
  (let ((possible (aref grid i j)))
    ;(print 'flood)
    (loop for x below 9
       if (/= i x)
       do (loop for y below 9
  	     if (/= j y)
  	     do (setf possible (set-difference possible
  					       (aref grid x y)))))
    (if (null (cdr possible))
  	(car possible)
  	nil)))

(defun exclusive-strat (grid)
    (let ((ret nil))
      (loop
	 for i below 9 always (null ret)
	 do (loop
	       for j below 9 always (null ret)
	       do (if (and (null (cdr (aref grid i j)))
			   (not (null (car (aref grid i j)))))
		      (progn (setf ret (list i
					     j
					     (car (aref grid i j))))
			     ;(print 'teiurs)
			     (supprimer-colone grid (caddr ret) j)
			     (supprimer-ligne grid (caddr ret) i)
			     (supprimer-carre grid (caddr ret) i j)
			     (setf (aref grid i j) nil)))))
      (values-list ret)))
;; (defun pifo-strat (grid)
;;   (let ((ret nil))
;;     (loop for i below 9 aways (null
(defun inclusive-strat (grid)
  ;(print 'strat_inclusive)
    (let ((ret '(0 0 nil)))
      (loop for i below 9 always (null (caddr ret))
	 do (loop for j below 9 always (null (caddr ret))
	       do (progn (setf ret (list i
					 j
					 (inclusive-colone grid i j)))
			 ;(print ret)
			 (if (null (caddr ret))
			     
			       (setf ret (list i
					     j
					     (inclusive-ligne grid i j))))
			 ;(print ret)
			 (if (null (caddr ret))
				   
			     (setf ret (list i
					     j
					     (inclusive-carre grid i j))))
					;(print ret)
			 )))
      (if (null (caddr ret))
	  (progn (print '(ça explique pas mal de choses)) nil)
	  (progn (supprimer-colone grid (caddr ret) (cadr ret))
		 (supprimer-ligne grid (caddr ret) (car ret))
		 (supprimer-carre grid (caddr ret) (car ret) (cadr ret))
		 (setf (aref grid (car ret) (cadr ret)) nil)
		 (values-list ret)))))

(let ((pgrid nil)
      (travail-grid nil))
  (defun init-standalone (grid)
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
    (print pgrid))
  (defun main-standalone ()
    ;(print pgrid)
    (multiple-value-bind (i j v) (exclusive-strat pgrid)
      (if (not (null v))
	  (progn (values i j v))
	  (progn (inclusive-strat pgrid))))))

(defun test-strat (init main grid)
  (funcall init grid)
  (labels ((intern-test-strat ()
	     (multiple-value-bind (i j v) (time (funcall main))
	       (if (not (null v))
		   (progn (setf (aref grid i j) v)
			  (afficher-sudoku grid)
			  (intern-test-strat))))))
    (intern-test-strat)
    (test)))
;    (afficher-sudoku grid)))
