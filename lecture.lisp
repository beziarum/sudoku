

(defun transformation (c)
  (labels ((tInter (c n l)
	   (cond ((eq (car l) c) n)
		 ((null (cdr l)) n)
		 (T (tInter c (1+ n) (cdr l))))))
    (tInter c 0 '(A B C D E F G H I))))
