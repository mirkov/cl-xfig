(in-package :cl-xfig)

(defun make-arrow (&key
		   (type *arrow-type*)
		   (style *arrow-style*)
		   (thickness *arrow-thickness*)
		   (width *arrow-width*)
		   (height *arrow-height*))
  (make-instance 'arrow-def
		 :arrow-type (getf *arrow-types* type)
		 :arrow-style (getf *arrow-styles* style)
		 :arrow-thickness thickness
		 :arrow-width width
		 :arrow-height height))

(defun make-polyline (points
		      &key
		      (line-style *line-style*)
		  (thickness *thickness*)
		  (pen-color *pen-color*)
		  (fill-color *fill-color*)
		  (depth *depth*)
		  (pen-style *pen-style*)
		  (area-fill *area-fill*)
		  (style-val *style-val*)
		  (join-style *join-style*)
		  (cap-style  *cap-style*)
		  (forward-arrow *forward-arrow*)
		  (backward-arrow *backward-arrow*))
  (make-instance 'polyline
		 :sub-type 1
		 :point-coords points
		 :npoints (length points)
		  :line-style line-style
		  :line-thickness thickness
		  :pen-color pen-color
		  :fill-color fill-color
		  :depth depth
		  :pen-style pen-style
		  :area-fill area-fill
		  :style-val style-val
		  :join-style join-style
		  :cap-style  cap-style
		  :radius 0
		  :forward-arrow-def forward-arrow
		  :backward-arrow-def backward-arrow))

(defun make-rectangle (x-min x-max y-min y-max
		       &key 		      (line-style *line-style*)
		  (thickness *thickness*)
		  (pen-color *pen-color*)
		  (fill-color *fill-color*)
		  (depth *depth*)
		  (pen-style *pen-style*)
		  (area-fill *area-fill*)
		  (style-val *style-val*)
		  (join-style *join-style*))
  (make-instance 'polyline
		 :sub-type 2
		 :point-coords `((,x-min ,y-min)
				 (,x-max ,y-min)
				 (,x-max ,y-max)
				 (,x-min ,y-max)
				 (,x-min ,y-min))
		 :npoints 5
		  :line-style line-style
		  :line-thickness thickness
		  :pen-color pen-color
		  :fill-color fill-color
		  :depth depth
		  :pen-style pen-style
		  :area-fill area-fill
		  :style-val style-val
		  :join-style join-style
		  :cap-style  0
		  :radius 0
		  :forward-arrow-def nil
		  :backward-arrow-def nil))
