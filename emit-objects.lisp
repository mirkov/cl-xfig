(in-package :cl-xfig)

(defgeneric emit-fig-object (fig-object &optional stream)
  (:documentation "Emit fig-object to stream"))

(defmethod emit-fig-object :before ((fig-object fig-object)
				    &optional (stream t))
  (princ "
" stream))
  
#+skip (defmethod emit-fig-object :after ((fig-object fig-object)
				    &optional (stream t))
  (princ "
" stream))
  

(defmethod emit-fig-object ((polyline polyline) &optional (stream t))
  (mapc (lambda (accessor)
	  (princ (funcall accessor polyline)
		 stream)
	  (princ " " stream))
	(list #'object-code
#'sub-type
#'line-style
#'line-thickness
#'pen-color
#'fill-color
#'depth
#'pen-style
#'area-fill
#'style-val
#'join-style
#'cap-style
#'radius
(lambda (polyline)
  (if (forward-arrow-def polyline) 1 0))
(lambda (polyline)
  (if (backward-arrow-def polyline) 1 0))
#'npoints))
(awhen (forward-arrow-def polyline)
  (princ "
" stream)
  (emit-fig-object it stream))
(awhen (backward-arrow-def polyline)
  (princ "
" stream)
  (emit-fig-object it stream))
(princ "
" stream)
(format stream "~{~{~a ~a~} ~}~%"
	(mapcar #'identity (point-coords polyline)))
(values))

(defmethod emit-fig-object ((arrow-def arrow-def) &optional (stream t))
  (mapc (lambda (accessor)
	  (princ (funcall accessor arrow-def)
		 stream)
	  (princ " " stream))
	'(arrow-type arrow-style arrow-thickness arrow-width arrow-height))
  (values))
