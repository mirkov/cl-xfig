(in-package :cl-xfig)

(defparameter *max-pixel* 60000
  "Specifies the maximum permissible pixel coordinate.  I want to
  prevent figures from being too large.  Output routines should check
  against it.")

;; Default figure parameters used by the figure header
(defvar *orientation* "Landscape")
(defvar *justification* "Flush Left")
(defvar *units* "Inches")
(defvar *papersize* "Letter")
(defvar *magnification* 100)
(defvar *multiple-page* "Single")
(defvar *transparent*-color -2)
(defvar *resolution* 1200)
(defvar *coord_system* 2)

(defun emit-xfig-header (stream)
  (format stream "#FIG 3.2~%")
;;  (format stream "~%")
  (format stream "~a ~%" *orientation*)
  (format stream "~a ~%" *justification*)
  (format stream "~a ~%" *units*)
  (format stream "~a ~%" *papersize*)
  (format stream "~a ~%" *magnification*)
  (format stream "~a ~%" *multiple-page*)
  (format stream "~a ~%" *transparent*-color)
  (format stream "~a ~a~%" *resolution* *coord_system*))

