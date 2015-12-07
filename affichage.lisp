;(in-package :sudoku)

(defun afficher-sudoku (tab)
  (afficher-case " ")
  (loop
     for l in '((A B C)(D E F)(G H I))
     do (progn (format t "|")
	       (loop
		  for i in l
		  do (afficher-case i))))
  (format t "|~%")
  (loop
     for i below 9
     if (= (mod i 3) 0)
     do (progn (afficher-separation-ligne)
	       (afficher-ligne tab i))
     else
     do (afficher-ligne tab i))
  (afficher-separation-ligne))


(defun afficher-case (v)
  (format t " ~D " v))

(defun afficher-separation-ligne ()
  (format t (make-string 34 :initial-element #\*))
  (format t "~%"))

(defun afficher-ligne (tab i)
  (afficher-case (1+ i))
  (loop
     for j below 9
     if (= (mod j 3) 0)
     do (progn (format t "|")
	       (afficher-case (aref tab i j)))
     else
     do (afficher-case (aref tab i j)))
  (format t "|~%"))
