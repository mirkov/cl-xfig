(in-package :cl-xfig)

;;; base object
(defclass fig-object ()
  ((object-code :reader object-code
	 :documentation "The object code"))
  (:documentation "Base object for all xfig objects"))



;; the following is based on a post by Pascal Constanza on 11/11/09 and followups:
;; - https://groups.google.com/d/msg/comp.lang.lisp/TsmbsOK32DM/9Q7prvFV9YsJ
;; - https://groups.google.com/d/msg/comp.lang.lisp/TsmbsOK32DM/AgVgz9MveJAJ

(defclass checked-class (standard-class)
  ())
#+sbcl(defmethod sb-mop:validate-superclass ((class checked-class)
				       (superclass standard-class))
  t)
#+clisp(defmethod validate-superclass ((class checked-class)
				       (superclass standard-class))
  t)


(defmethod (setf #+sbcl sb-mop:slot-value-using-class
		 #+clisp slot-value-using-class) :before
    (new-value (class checked-class) object slot)
  (let ((class-name (class-name (class-of object)))
	(slot-name (#+sbcl sb-mop:slot-definition-name
			   #+clisp slot-definition-name slot)))
  (assert (typep new-value (#+sbcl sb-mop:slot-definition-type
				   #+clisp slot-definition-name slot)) ()
	  "Slot ~a of class ~a cannot accept value ~a" slot-name class-name
	  new-value)))


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
	 :type arrow-type)
   (style :reader arrow-style
	  :initarg :arrow-style
	  :type arrow-style)
   (thickness :reader arrow-thickness
	      :initarg :arrow-thickness
	      :type float>0)
   (width :reader arrow-width
	  :initarg :arrow-width
	  :type float>0)
   (height :reader arrow-height
	   :initarg :arrow-height
	   :type float>0))
  (:documentation "Define arrow")
  (:metaclass checked-class))


(defclass terminated-line-mixin ()
  ((cap-style :reader cap-style
	      :initarg :cap-style
	      :type cap-style)
   (forward-arrow-def :initarg :forward-arrow-def
		      :reader forward-arrow-def
		      :type arrow-def1)
   (backward-arrow-def :initarg :backward-arrow-def
		      :reader backward-arrow-def
		      :type arrow-def1))
  (:documentation "Mixin for a terminated line (that does not close on itself"))

(defclass pen-mixin ()
  ((line-style :reader line-style
	       :initarg :line-style
	       :type line-style)
   (line-thickness :reader line-thickness
		   :initarg :line-thickness
		   :type integer>=0)
   (pen-color :reader pen-color
	      :initarg :pen-color
	      :type pen-color)
   (fill-color :reader fill-color
	       :initarg :fill-color
	       :type fill-color)
   (pen-style :reader pen-style
	      :initarg :pen-style
	      :type integer)
   (area-fill :reader area-fill
	      :initarg :area-fill
	      :type area-fill)
   (style-val :reader style-val
	      :initarg :style-val
	      :type float))
  (:documentation "mixin with line properties"))



;;; graphic objects
(defclass arc (fig-object drawable-object-mixin
			  terminated-line-mixin)
  ((object-code :initform 5
		:allocation :class)
   (direction :initarg :direction
	      :reader direction
	      :type arc-direction)
   (center-x :reader center-x
	     :initarg :center-x
	     :type float)
   (center-y :reader center-y
	     :initarg :center-y
	     :type float)
   (x1 :reader x1 :initarg :x1
       :type integer)
   (x2 :reader x2 :initarg :x2
       :type integer)
   (x3 :reader x3 :initarg :x3
       :type integer)
   (y1 :reader y1 :initarg :y1
       :type integer)
   (y2 :reader y2 :initarg :y2
       :type integer)
   (y3 :reader y3 :initarg :y3
       :type integer))
  (:documentation "Stores definition of an arc")
  (:metaclass checked-class))

(defclass ellipse (fig-object drawable-object-mixin
			  line-mixin)
  ((object-code :initform 1
		:allocation :class)
   (direction :initform 1
	      :allocation :class)
   (angle :initarg :angle
	  :reader angle
	  :type float)
   (center-x :reader center-x
	     :initarg :center-x
	     :type float)
   (center-y :reader center-y
	     :initarg :center-y
	     :type float)
   (start-x :reader start-x :initarg :start-x
       :type integer)
   (end-x :reader end-x :initarg :end-x
       :type integer)
   (start-y :reader start-y :initarg :start-y
       :type integer)
   (end-y :reader end-y :initarg :end-y
       :type integer))
  (:documentation "Stores definition of an ellipse")
  (:metaclass checked-class))

(defclass polyline (fig-object drawable-object-mixin
			       pen-mixin
			       terminated-line-mixin)
  ((object-code :initform 2
		:allocation :class)
   (join-style :initarg :join-style
	       :reader join-style
	       :type join-style)
   (point-coords :initarg :point-coords
		 :reader point-coords)
   (npoints :initarg :npoints
	    :reader npoints
	    :type integer)
   (radius :initarg :radius
	   :reader radius
	   :type integer))
  (:documentation "Polyline definition")
  (:metaclass checked-class))
