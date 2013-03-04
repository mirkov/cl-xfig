(in-package :cl-xfig)

(defmacro in-cl-xfig-dir (&body body)
  `(let ((*default-pathname-defaults*
	  (asdf:system-source-directory "cl-xfig")))
     ,@body))

(defmacro with-output-to-fig-file ((stream file) &body body)
  `(with-output-to-file (,stream ,file
				 :if-exists :supersede)
     (emit-xfig-header ,stream)
     ,@body))


(define-test line-with-arrow
  (in-cl-xfig-dir
    (with-output-to-file (stream "line-with-arrow.fig"
				 :if-exists :supersede) 
      (let ((arrow (make-arrow)))
	(emit-xfig-header stream)
	(emit-fig-object (make-polyline 
			  '((0 1000) (3000 2000) (4000 5000) (7000 6000))
			  :forward-arrow arrow)
			 stream)))))

(define-test rectangle
  (in-cl-xfig-dir
    (with-output-to-fig-file (stream "rectangle.fig")
      (emit-fig-object (make-rectangle 0 1000 2000 3000) stream))))
