(in-package :cl-xfig)

;;; base object
(defclass fig-object ()
  ((object-code :reader object-code
	 :documentation "The object code"))
  (:documentation "Base object for all xfig objects"))

;;; mixin's
(defclass drawable-object-mixin ()
  ((sub-type :reader sub-type
	     :initarg :sub-type
	     :documentation "Object sub-type")
   (depth :reader depth
	  :initarg :depth
	  :type 'depth))
  (:documentation "Common properties to all explicitly drawable objects"))

(defclass arrow-def ()
  ((type :reader arrow-type
	 :initarg :arrow-type
	 :type 'arrow-type)
   (style :reader arrow-style
	  :initarg :arrow-style
	  :type 'arrow-styles)
   (thickness :reader arrow-thickness
	      :initarg :arrow-thickness
	      :type 'float>0)
   (width :reader arrow-width
	  :initarg :arrow-width
	  :type 'float>0)
   (height :reader arrow-height
	   :initarg :arrow-height
	   :type 'float>0))
  (:documentation "Define arrow"))


(defclass terminated-line-mixin ()
  ((cap-style :reader cap-style
	      :initarg :cap-style
	      :type 'cap-style)
   (forward-arrow-def :initarg :forward-arrow-def
		      :reader forward-arrow-def
		      :type 'arrow-def)
   (backward-arrow-def :initarg :backward-arrow-def
		      :reader backward-arrow-def
		      :type 'arrow-def))
  (:documentation "Mixin for a terminated line (that does not close on itself"))

(defmethod initialize-instance :after ((self terminated-line-mixin)
				       &key cap-style)
  (assert (getf *cap-styles* cap-style))
  (setf (slot-value self 'cap-style) cap-style))


(defclass pen-mixin ()
  ((line-style :reader line-style
	       :initarg :line-style
	       :type 'line-style)
   (line-thickness :reader line-thickness
		   :initarg :line-thickness
		   :type 'integer>=0)
   (pen-color :reader pen-color
	      :initarg :pen-color
	      :type 'pen-color)
   (fill-color :reader fill-color
	       :initarg :fill-color
	       :type 'fill-color)
   (pen-style :reader pen-style
	      :initarg :pen-style
	      :type 'integer)
   (area-fill :reader area-fill
	      :initarg :area-fill
	      :type 'area-fill)
   (style-val :reader style-val
	      :initarg :style-val
	      :type 'float))
  (:documentation "mixin with line properties"))



;;; graphic objects
(defclass arc (fig-object drawable-object-mixin
			  terminated-line-mixin)
  ((object-code :initform 5
		:allocation :class)
   (direction :initarg :direction
	      :reader direction
	      :type 'arc-direction)
   (center-x :reader center-x
	     :initarg :center-x
	     :type 'float)
   (center-y :reader center-y
	     :initarg :center-y
	     :type 'float)
   (x1 :reader x1 :initarg :x1
       :type 'integer)
   (x2 :reader x2 :initarg :x2
       :type 'integer)
   (x3 :reader x3 :initarg :x3
       :type 'integer)
   (y1 :reader y1 :initarg :y1
       :type 'integer)
   (y2 :reader y2 :initarg :y2
       :type 'integer)
   (y3 :reader y3 :initarg :y3
       :type 'integer))
  (:documentation "Stores definition of an arc"))

(defclass ellipse (fig-object drawable-object-mixin
			  line-mixin)
  ((object-code :initform 1
		:allocation :class)
   (direction :initform 1
	      :allocation :class)
   (angle :initarg :angle
	  :reader angle
	  :type 'float)
   (center-x :reader center-x
	     :initarg :center-x
	     :type 'float)
   (center-y :reader center-y
	     :initarg :center-y
	     :type 'float)
   (start-x :reader start-x :initarg :start-x
       :type 'integer)
   (end-x :reader end-x :initarg :end-x
       :type 'integer)
   (start-y :reader start-y :initarg :start-y
       :type 'integer)
   (end-y :reader end-y :initarg :end-y
       :type 'integer))
  (:documentation "Stores definition of an ellipse"))

(defclass polyline (fig-object drawable-object-mixin
			       pen-mixin
			       terminated-line-mixin)
  ((object-code :initform 2
		:allocation :class)
   (join-style :initarg :join-style
	       :reader join-style
	       :type 'join-style)
   (point-coords :initarg :point-coords
		 :reader point-coords)
   (npoints :initarg :npoints
	    :reader npoints
	    :type 'integer)
   (radius :initarg :radius
	   :reader radius
	   :type 'integer))
  (:documentation "Polyline definition"))