(defvar *pack-probability* '(.1 .2 .4 .2 .1))
(defvar *cherry-per-pack* '(1 .75 .5 .25 .0))

(defun hypothesis-posterior-prob-unnormalized (i limes cherries)
  (let* ((hi (elt *cherry-per-pack* i))
	 (likelihood (* (expt (- 1 hi) limes)
			(expt hi cherries)))
	 (hypothesis-prior (elt *pack-probability* i)))
    (* likelihood hypothesis-prior)))

(defun hypothesis-posterior-prob (i limes cherries)
  (/ (hypothesis-posterior-prob-unnormalized i limes cherries)
     (loop for i below (length *pack-probability*) sum
	  (hypothesis-posterior-prob-unnormalized i limes cherries))))

(progn 
  (with-open-file (s "/dev/shm/candy.dat" :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (loop for l upto 10 do
	(format s "~d " l)
	(loop for i below (length *pack-probability*) do
	     (format s "~d " (hypothesis-posterior-prob i l 0)))
	(terpri s)))
  (with-open-file (p "/dev/shm/candy.gp" :direction :output
		     :if-exists :supersede :if-does-not-exist :create)
    (format p "set grid;
plot \"/dev/shm/candy.dat\" u 1:2 w l, \"/dev/shm/candy.dat\" u 1:3 w l, \"/dev/shm/candy.dat\" u 1:4 w l, \"/dev/shm/candy.dat\" u 1:5 w l, \"/dev/shm/candy.dat\" u 1:6 w l; pause -1;"))
  (sb-ext:run-program "/usr/bin/gnuplot" '("/dev/shm/candy.gp")))

(defun next-candy-is-lime-prob (limes cherries)
  (loop for i below (length *pack-probability*) sum
       (let ((draw-lime-prob (- 1 (elt *cherry-per-pack* i))))
	 (* draw-lime-prob (hypothesis-posterior-prob i limes cherries)))))

(loop for i upto 10 collect
 (next-candy-is-lime-prob i 0))