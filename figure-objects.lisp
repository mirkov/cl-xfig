(in-package :cl-xfig)

;;; base object
(defclass fig-object ()
  ((object-code :reader object-code
	 :documentation "The object code")
   (sub-type :reader sub-type
	     :initarg :sub-type
	     :documentation "Object sub-type"))
  (:documentation "Base object for all xfig objects"))

;;; mixin's
(defclass drawable-object-mixin ()
  ((depth :reader depth
	  :initarg :depth))
  (:documentation "Common properties to all explicitly drawable objects"))

(defclass arrow-def ()
  ((type :reader arrow-type
	       :initarg :arrow-type)
   (style :reader arrow-style
		:initarg :arrow-style)
   (thickness :reader arrow-thickness
	      :initarg :arrow-thickness)
   (width :reader arrow-width
	  :initarg :arrow-width)
   (height :reader arrow-height
	   :initarg :arrow-height))
  (:documentation "Define arrow"))


(defclass terminated-line-mixin ()
  ((cap-style :reader cap-style
	      :initarg :cap-style)
   (forward-arrow-def :initarg :forward-arrow-def
		      :reader forward-arrow-def)
   (backward-arrow-def :initarg :backward-arrow-def
		      :reader backward-arrow-def))
  (:documentation "Mixin for a terminated line (that does not close on itself"))


(defclass line-mixin ()
  ((line-style :reader line-style
	       :initarg :line-style)
   (line-thickness :reader line-thickness
		   :initarg :line-thickness)
   (pen-color :reader pen-color
	      :initarg :pen-color)
   (fill-color :reader fill-color
	       :initarg :fill-color)
   (pen-style :reader pen-style
	      :initarg :pen-style)
   (area-fill :reader area-fill
	      :initarg :area-fill)
   (style-val :reader style-val
	      :initarg :style-val))
  (:documentation "mixin with line properties"))

;;; graphic objects
(defclass arc (fig-object drawable-object-mixin
			  terminated-line-mixin)
  ((object-code :initform 5
		:allocation :class)
   (direction :initarg :direction
	      :reader direction)
   (center-x :reader center-x
	     :initarg :center-x)
   (center-y :reader center-y
	     :initarg :center-y)
   (x1 :reader x1 :initarg :x1)
   (x2 :reader x2 :initarg :x2)
   (x3 :reader x3 :initarg :x3)
   (y1 :reader y1 :initarg :y1)
   (y2 :reader y2 :initarg :y2)
   (y3 :reader y3 :initarg :y3))
  (:documentation "Stores definition of an arc"))

(defclass ellipse (fig-object drawable-object-mixin
			  line-mixin)
  ((object-code :initform 1
		:allocation :class)
   (direction :initform 1
	      :allocation :class)
   (angle :initarg :angle
	  :reader angle)
   (center-x :reader center-x
	     :initarg :center-x)
   (center-y :reader center-y
	     :initarg :center-y)
   (start-x :reader start-x :initarg :start-x)
   (end-x :reader end-x :initarg :end-x)
   (start-y :reader start-y :initarg :start-y)
   (end-y :reader end-y :initarg :end-y))
  (:documentation "Stores definition of an ellipse"))

(defclass polyline (fig-object drawable-object-mixin
			       line-mixin
			       terminated-line-mixin)
  ((object-code :initform 2
		:allocation :class)
   (join-style :initarg :join-style
	       :reader join-style)
   (point-coords :initarg :point-coords
		 :reader point-coords)
   (npoints :initarg :npoints
	    :reader npoints)
   (radius :initarg :radius
	   :reader radius))
  (:documentation ""))