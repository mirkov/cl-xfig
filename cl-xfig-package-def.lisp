(defpackage :cl-xfig
  (:use :cl :lisp-unit)
  (:import-from :anaphora
		:it
		:awhen)
  (:import-from :alexandria
		:with-output-to-file)
  #+skip(:export :dump-xfig-header 
	   :poly-line :dump-poly-line
	   :arc :dump-arc
	   :circle :dump-circle
	   :spline :dump-spline :roi :sharpy-roi
	   :*output*))
