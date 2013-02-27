;; package for producing xfig files.  It consists of utilities that
;; produce the file itself, plus some simple wrapper utilities that
;; (like the circle which is a specialization of xfig's ellipse, or
;; roi which is a specialization of xfig's closed approximated
;; spline. 

;; The philosophy is semi-object oriented.  I do not use objects.
;; Each object is specified as a p-list, and it has a print function.
;; The typical usage is
;; (dump-<object> (<object> definition parameters))

(defpackage :cl-xfig
  (:use :cl)
  (:export :dump-xfig-header 
	   :poly-line :dump-poly-line
	   :arc :dump-arc
	   :circle :dump-circle
	   :spline :dump-spline :roi :sharpy-roi
	   :*output*))

(in-package :cl-xfig)

;;;; general definitions that will be applicable for all of the
;;;; package

(defparameter *output* t "output stream.  Set to 't', so that other
code can overwrite it by specifying *standard-output*")

(defparameter *max-pixel* 60000
  "Specifies the maximum permissible pixel coordinate.  I want to
  prevent figures from being too large.  Output routines should check
  against it.")

;; Default figure parameters used by the figure header
(defparameter *orientation* "Landscape")
(defparameter *justification* "Flush Left")
(defparameter *units* "Inches")
(defparameter *papersize* "Letter")
(defparameter *magnification* 100)
(defparameter *multiple-page* "Single")
(defparameter *transparent*-color -2)
(defparameter *resolution* 1200)
(defparameter *coord_system* 2)


(defparameter *color-index* 32 "Index of available custom color.  To
be incremented after each new color printout (not definition)")

(defparameter *object-codes*
  (list
   :color-def 0
   :ellipse 1
   :poly-line 2
   :spline 3
   :text 4
   :arc 5
   :compound 6))

;; Parameters specifying the drawing style.  I first specify a list of
;; allowed values, follwed by the default value.  The default values
;; are used in the keyword lists for the object definition.

(defparameter *line-styles*
  (list
   :default -1
   :solid 0
   :dashed 1
   :dotted 2
   :dash-dotted 3
   :dash-double-dotted 4
   :dash-triple-dotted 5)
  "Available line styles")

(defparameter *line-style* (getf *line-styles* :solid))
(defparameter *style-val* 0.01 "style value for dashed and dotted lines")

(defparameter *join-styles*
  (list
   :miter 0
   :round 1
   :bevel 2))
(defparameter *join-style* (getf *join-styles* :round))

(defparameter *cap-styles*
  (list
   :butt 0
   :round 1
   :projecting 2))
(defparameter *cap-style* (getf *cap-styles* :butt))

;; arrow styles
;;  0 = Stick-type (the default in xfig 2.1 and earlier)
;;
;; 		 \
;; 		    \
;; 	_______________\
;; 		       /
;; 		    /
;; 		 /
;;
;;  1 = Closed triangle:
;;
;; 		|\
;; 		|   \
;; 	________|      \
;; 		|      /
;; 		|   /
;; 		|/
;;
;;  2 = Closed with "indented" butt:
;;
;; 		|\
;; 		\   \
;; 		 \     \
;; 	__________\       \
;; 		  /       /
;; 		 /     /
;; 		/   /
;; 		|/
;;
;;  3 = Closed with "pointed" butt:
;;
;; 		   /\
;; 		  /    \
;; 		 /        \
;; 	________/            \
;; 		\            /
;; 		 \        /
;; 		  \    /
;; 		   \/
(defparameter *arrow-styles*
  (list
   :stick 0
   :closed-triangle 1
   :indented-butt 2
   :pointed-butt 3))

(defparameter *arrow-fill-styles*
  (list
   :hollow 0 ;; filled with white
   :filled 1))

;; color definitions
(defparameter *default-color* -1)
(defparameter *black* 0)
(defparameter *blue* 1)
(defparameter *green* 2)
(defparameter *cyan* 3)
(defparameter *red* 4)
(defparameter *magenta* 5)
(defparameter *yellow* 6)
(defparameter *white* 7)
(defparameter *blue-1* 8)
(defparameter *blue-2* 9)
(defparameter *blue-3* 10)
(defparameter *blue-4* 11)
(defparameter *green-1* 12)
(defparameter *green-3* 13)
(defparameter *green-2* 14)
(defparameter *cyan-1* 15)
(defparameter *cyan-2* 16)
(defparameter *cyan-3* 17)
(defparameter *red-1* 18)
(defparameter *red-2* 19)
(defparameter *red-3* 20)
(defparameter *magenta-1* 21)
(defparameter *magenta-2* 22)
(defparameter *magenta-3* 23)
(defparameter *brown-1* 24)
(defparameter *brown-2* 25)
(defparameter *brown-3* 26)
(defparameter *pink-1* 27)
(defparameter *pink-2* 28)
(defparameter *pink-3* 29)
(defparameter *pink-4* 30)
(defparameter *gold* 31)

(defparameter *colors*
  (list 
   (cons 'default-color -1)
   (cons 'black 0)
   (cons 'blue 1)
   (cons 'green 2)
   (cons 'cyan 3)
   (cons 'red 4)
   (cons 'magenta 5)
   (cons 'yellow 6)
   (cons 'white 7)
   (cons 'blue-1 8)
   (cons 'blue-2 9)
   (cons 'blue-3 10)
   (cons 'blue-4 11)
   (cons 'green-1 12)
   (cons 'green-3 13)
   (cons 'green-2 14)
   (cons 'cyan-1 15)
   (cons 'cyan-2 16)
   (cons 'cyan-3 17)
   (cons 'red-1 18)
   (cons 'red-2 19)
   (cons 'red-3 20)
   (cons 'magenta-1 21)
   (cons 'magenta-2 22)
   (cons 'magenta-3 23)
   (cons 'brown-1 24)
   (cons 'brown-2 25)
   (cons 'brown-3 26)
   (cons 'pink-1 27)
   (cons 'pink-2 28)
   (cons 'pink-3 29)
   (cons 'pink-4 30)
   (cons 'gold 31)
   (cons 'gray-1 32)
   (cons 'gray-2 33)
   (cons 'gray-3 34)
   (cons 'gray-4 35)
   ))



;; additional custom colors
(defparameter *gray-1* "#cccccc")
(defparameter *gray-2* "#999999")
(defparameter *gray-3* "#666666")
(defparameter *gray-4* "#333333")


;; default drawing style
(defparameter *thickness* 1)
(defparameter *pen-color* 'black)
(defparameter *fill-color* 7)
(defparameter *depth* 50)
(defparameter *pen-style* -1 "Not used by xfig")
(defparameter *area-fill* -1)
(defparameter *radius* -1)
(defparameter *forward-arrow* 0)
(defparameter *backward-arrow* 0)


;;;; general output utilities

;; This function should be called by all output to scale from inches
;; to pixels
(defun rescale (coord)
  "Rescales coordinate (assumed in inches) to pixels"
  (let ((px (round (* coord *resolution*))))
    (when (> (abs px) *max-pixel*)
      (error "Pixel value exceeded maximum allowed"))
    px))


;;;; File generation functions
(defun dump-xfig-header ()
  (format *output* "#FIG 3.2~%")
;;  (format *output* "~%")
  (format *output* "~a ~%" *orientation*)
  (format *output* "~a ~%" *justification*)
  (format *output* "~a ~%" *units*)
  (format *output* "~a ~%" *papersize*)
  (format *output* "~a ~%" *magnification*)
  (format *output* "~a ~%" *multiple-page*)
  (format *output* "~a ~%" *transparent*-color)
  (format *output* "~a ~a~%" *resolution* *coord_system*))




;;;; geometric objects

;;; color-def object -- remember, it has to be printed out before all
;;; other objects.
(defun color-def (hex-value)
  (list (getf *object-codes* :color-def) *color-index* hex-value))

(defun dump-color-def (color-def)
  (format *output* "~{~a ~}~%" color-def)
  (1+ *color-index*))


;;; poly-line object
(defparameter *poly-line-types*
  (list
   :polyline 1
   :box 2
   :polygon 3
   :arc-box 4
   :imported-picture 5))

(defparameter *poly-line-type* (getf *poly-line-types* :polyline)
  "default poly-line type")

(defun poly-line (points-list &key
		  (type *poly-line-type*)
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
		  (radius *radius*)
		  (forward-arrow *forward-arrow*)
		  (backward-arrow *backward-arrow*))
  (let ((arg-count (list-length points-list)))
    (if (< arg-count 4) (error "require four or more points"))
    (if (oddp arg-count) (error "require even number of points")))
  (let ((npoints (/ (list-length points-list) 2)))
    (list
     :object-code (getf *object-codes* :poly-line)
     :sub-type type
     :line-style line-style
     :thickness thickness
     :pen-color (cdr (assoc pen-color *colors*))
     :fill-color fill-color
     :depth depth
     :pen-style pen-style
     :area-fill area-fill
     :style-val style-val
     :join-style join-style
     :cap-style  cap-style
     :radius radius
     :forward-arrow forward-arrow
     :backward-arrow backward-arrow
     :npoints npoints
     :points points-list)
     ))

(defun dump-poly-line (poly-line)
  ;; dump the poly-line info
  ;; We first print all the elements but last (the points)
  ;; the format skips every other entry ~* (the property names)
  
  ;; if we use (format *output* ... I get a nil printed out
    (format *output* "~{~*~a ~}~%" (subseq poly-line 0 32))
    (format *output* "~{~a ~}~%" (mapcar #'rescale (getf poly-line :points))))

;;; arc object

(defparameter *arc-types*
  (list
   :open 1
   :pie-wedge 2))

(defparameter *arc-type* (getf *arc-types* :open))

(defun arc (points-list &key
	    (type *arc-type*)
	    (line-style *line-style*)
	    (thickness *thickness*)
	    (pen-color *pen-color*)
	    (fill-color *fill-color*)
	    (depth *depth*)
	    (pen-style *pen-style*)
	    (area-fill *area-fill*)
	    (style-val *style-val*)
	    (cap-style *cap-style*)
	    (direction 0)
	    (forward-arrow 0)
	    (backward-arrow 0))

  (when (not (eql (list-length points-list) 6))
    (error "require three points"))
  (list
   :object-code (getf *object-codes* :arc)
   :sub-type type
   :line-style line-style
   :thickness thickness
   :pen-color (cdr (assoc pen-color *colors*))
   :fill-color fill-color
   :depth depth
   :pen-style pen-style
   :area-fill area-fill
   :style-val style-val
   :cap-style cap-style
   :direction direction
   :forward-arrow forward-arrow
   :backward-arrow backward-arrow
   :center (arc-center points-list)
   :points points-list))

(defun arc-center (point-list)
  "calculate ellipse center -- transcribed from xfig's code u_geom.c:
  compute_arccenter"
  (destructuring-bind (x1 y1 x2 y2 x3 y3) point-list
    (when (or (and (eql x1 x3) (eql y1 y3))
	    (and (eql x1 x2) (eql y1 y2))
	    (and (eql x2 x3) (eql y2 y3)))
      (error "At least two points are coincident"))
    (let (cx
	  cy
	  (a (- x1 x2))
	  (b (+ x1 x2))
	  (c (- y1 y2))
	  (d (+ y1 y2))
	  (f (- x2 x3))
	  (g (+ x2 x3))
	  (h (- y2 y3))
	  (i (+ y2 y3)))
      (let ((e (/ (+ (* a b) (* c d)) 2))
	    (j (/ (+ (* f g) (* h i)) 2)))
	(if (not (eql 0 (- (* a h) (* c f))))
	    (setf cy (/ (- (* a j) (* e f))
			(- (* a h) (* c f))))
	    (error "cannot compute y-center -- test 1 failed"))
	(if (not (eql 0 a))
	    (setf cx (/ (- e (* cy c)) a))
	    (if (not (eql 0 f))
		(setf cx (/ (- j (* cy h)) f))
		(error "cannot compute x-center, both a&f are zero"))))
      (list (float cx) (float cy)))))
	  

(defun dump-arc (arc)
  ;; dump the poly-line info
  ;; We first print all the elements but last (the points)
  ;; the format skips every other entry ~* (the property names)
  
  ;; if we use (format *output* ... I get a nil printed out
    (format *output* "~{~*~a ~}" (subseq arc 0 28))
    (format *output* "~{~a ~}" (mapcar #'rescale (getf arc :center)))
    (format *output* "~{~a ~}~%" (mapcar #'rescale (getf arc :points))))

;;; Ellipse and ellipse based objects  
(defparameter *ellipse-types*
  (list
   :ellipse-defined-by-radii 1
   :ellipse-defined-by-diameters 2
   :circle-defined-by-radius 3
   :circle-defined-by-diameter 4))

(defparameter *ellipse-type* (getf *ellipse-types* :circle-defined-by-radius))

;; circle defined by center and radius
(defun circle (center radius &key (line-style *line-style*)
	       (thickness *thickness*)
	       (pen-color *pen-color*)
	       (fill-color *fill-color*)
	       (depth *depth*)
	       (pen-style *pen-style*)
	       (area-fill *area-fill*)
	       (style-val *style-val*))
  (list
   :object-code (getf *object-codes* :ellipse)
   :sub-type (getf *ellipse-types* :circle-defined-by-radius)
   :line-style line-style
   :thickness thickness
   :pen-color (cdr (assoc pen-color *colors*))
   :fill-color fill-color
   :depth depth
   :pen-style pen-style
   :area-fill area-fill
   :style-val style-val
   :direction 1
   :angle 0.0
   :center center
   :radius radius))

(defun dump-circle (circle)
    (format *output* "~{~*~a ~}" (subseq circle 0 24))
    (format *output* "~{~a ~}" (mapcar #'rescale (getf circle :center)))
    ;; this section produces the missing values that are good enough
    ;; for a circle
    (format *output* "~{~a ~}~%"
	    (mapcar #'rescale
		   (destructuring-bind (cx cy) (getf circle :center)
		     (let ((r (getf circle :radius)))
		       (let* ((rx r)
			      (ry r)
			      (startx cx)
			      (starty cy)
			      (endx (+ cx r))
			      (endy cy))
			 (list rx ry startx starty endx endy)))))))
	
;;; spline-based objects

(defparameter *spline-types*
  (list
   :closed-approximated-spline 1
   :open-interpolated-spline 2
   :closed-interpolated-spline 3
   :open-x-spline 4
   :closed-x-spline 5))

(defparameter *spline-type* (getf *spline-types* :open-x-spline))

(defun spline (points-list &key
	       (type *spline-type*)
	       (line-style *line-style*)
	       (thickness *thickness*)
	       (pen-color *pen-color*)
	       (fill-color *fill-color*)
	       (depth *depth*)
	       (pen-style *pen-style*)
	       (area-fill *area-fill*)
	       (style-val *style-val*)
	       (cap-style *cap-style*)
	       (forward-arrow 0)
	       (backward-arrow 0))
  (list
   :object-code (getf *object-codes* :spline)
   :sub-type type
   :line-style line-style
   :thickness thickness
   :pen-color (let ((color-index (cdr (assoc pen-color *colors*))))
		(if color-index color-index 0))
   :fill-color fill-color
   :depth depth
   :pen-style pen-style
   :area-fill area-fill
   :style-val style-val
   :cap-style cap-style
   :forward-arrow forward-arrow
   :backward-arrow backward-arrow
   :npoints (length points-list)
   :points points-list))

	  

(defun dump-spline (spline)
  ;; dump the poly-line info
  ;; We first print all the elements but last (the points)
  ;; the format skips every other entry ~* (the property names)
    (format *output* "~{~*~a ~}~%" (subseq spline 0 28))
    (format *output* "~{~a ~}~%"
	    (mapcar #'rescale
		    (loop for point in (getf spline :points)
			 collecting (first point)
			 collecting (second point))))
    (format *output* "~{~a ~}~%"
	    (mapcar #'third (getf spline :points))))
  
;; specialized spline objects
(defun roi (points &rest spline-kwrds &key &allow-other-keys)
  "Defines a bloby region of interest -- no sharp conrners"
  (apply #'spline
   (loop for point in points
      collecting (append point '(1.0)))
   :type (getf *spline-types* :closed-approximated-spline)
   :allow-other-keys t spline-kwrds))

(defun sharpy-roi (points &rest spline-kwrds
			  &key  &allow-other-keys)
  "Defines a curvi-angular ROI based on passed points.  Angular points
are defined by a three element list, third element having the value of
zero"
  (apply #' spline
   (loop for point in points
      collecting (append (subseq point 0 2)
			 (if (= (length point) 2)
			     '(1.0)
			     (list (float (third point))))))
   :type (getf *spline-types* :closed-approximated-spline)
   :allow-other-keys t spline-kwrds))
  