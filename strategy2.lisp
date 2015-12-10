(defconstant +carre-size+ 3)

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
			     (supprimer-colone grid (caddr ret) j)
			     (supprimer-ligne grid (caddr ret) i)
			     (supprimer-carre grid (caddr ret) i j)
			     (setf (aref grid i j) nil)))))
      (values-list ret)))

(defun simplifier-colonne (grid i) 
  (let ((simp nil))
    (loop for y1 below 9
       do (loop for y2 from (1+ y1) below 9
	     if (and (equal (aref grid i y1) (aref grid i y2))
		     (= (length (aref grid i y1)) 2))
	     do (loop for y3 below 9
		   if (and (/= y1 y2 y3)
			   (intersection (aref grid i y3)
					 (aref grid i y2)))
		   do (progn (setf (aref grid i y3)
				   (set-difference (aref grid i y3)
						   (aref grid i y2)))
			     (setf simp t)))))
    simp))

(defun simplifier-ligne (grid j)
  (let ((simp nil))
    (loop for x1 below 9
       do (loop for x2 from (1+ x1) below 9
	     if (and (equal (aref grid x1 j) (aref grid x2 j))
		     (= (length (aref grid x1 j)) 2))
	     do (loop for x3 below 9
		   if (and (/= x1 x2 x3)
			   (intersection (aref grid x3 j)
					 (aref grid x2 j)))
		   do (progn (setf (aref grid x3 j)
				   (set-difference (aref grid x3 j)
						   (aref grid x2 j)))
			     (setf simp t)))))
    simp))
;; on lui donne le coins supérieur gauche d'un carre
(defun simplifier-carre (grid i j)
  (let ((simp nil))
    (loop for x1 from i below (+ 3 i)
       do (loop for y1 from j below (+ 3 j)
	     do (loop for x2 from x1 below (+ 3 i)
		   do (loop for y2 from (if (= x1 x2) (+ 1 y1) j) below (+ 3 j) 
			 if (and (equal (aref grid x1 y1) (aref grid x2 y2))
				 (= (length (aref grid x1 y1)) 2))
			 do (loop for x3 from i below (+ 3 i)
			       do (loop for y3 from j below (+ 3 j)
				     if (and (and (or (/= x1 x3)
						      (/= y1 y3))
						  (or (/= x2 x3)
						      (/= y2 y3)))
					     (intersection (aref grid x3 y3)
							   (aref grid x2 y2)))
				     do (progn (setf (aref grid x3 y3)
						     (set-difference (aref grid x3 y3)
								     (aref grid x2 y2)))
					       (setf simp t))))))))
    simp))

(defun simplifier-sudoku (grid)
  (let ((simp nil))
    (loop for n below 9
       do (progn (setf simp (or (simplifier-colonne grid n) simp)) 
		 (setf simp (or (simplifier-ligne grid n) simp))
  		 (if (= (mod n 3) 0)
  		     (loop for m below 9 by 3
  		 	do (setf simp (or (simplifier-carre grid n m) simp))))))
    simp))


(defun inclusive-strat (grid)
    (let ((ret '(0 0 nil)))
      (loop for i below 9 always (null (caddr ret))
	 do (loop for j below 9 always (null (caddr ret))
	       do (progn (setf ret (list i
					 j
					 (inclusive-colone grid i j)))
			 (if (null (caddr ret))
			     
			       (setf ret (list i
					     j
					     (inclusive-ligne grid i j))))
			 (if (null (caddr ret))
				   
			     (setf ret (list i
					     j
					     (inclusive-carre grid i j)))))))
      (if (null (caddr ret))
	  nil
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
    (inclusive-strat pgrid))
  (defun main-standalone ()
    (let ((l (multiple-value-list (exclusive-strat pgrid))))
      (if (null (car l))
	  (setf l (multiple-value-list (inclusive-strat pgrid))))
      (if (and (null (car l))
	       (simplifier-sudoku pgrid))
	  (main-standalone)
	  (values-list l)))))


