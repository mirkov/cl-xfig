(in-package :cl-xfig)

;;; This file defines types for data stored in fig objects.

(defun index-matches (arg def)
  (member arg
	  (loop for (name index) on def by #'cddr
	      collect index)))
(defun >=0 (arg)
  (>= arg 0))


(defun matches-depth-p (arg)
  (and (> arg 0)
       (< arg 1000)))

(deftype depth-type ()
  `(and integer
	(satisfies matches-depth-p)))

(defun arrow-type-value-p (arg)
  (index-matches arg *arrow-types*))

(deftype arrow-type ()
  `(and integer
	(satisfies arrow-type-value-p)))

(defun match-arrow-style-p (arg)
  (index-matches arg *arrow-styles*))

(deftype arrow-style ()
  `(and integer
       (satisfies match-arrow-style-p)))

(deftype float>0 ()
  `(and float
       (satisfies >=0)))

(defun match-cap-style-p (arg)
  (index-matches arg *cap-styles*))

(deftype cap-style ()
  `(and integer
       (satisfies match-cap-style-p)))

(defun match-line-style-p (arg)
  (and (>= arg -1)
       (<= arg 5)))

(deftype line-style ()
  `(and integer
	(satisfies match-line-style-p)))

(deftype integer>=0 ()
  `(and integer
	(satisfies >=0)))

(defun match-pen-color-p (arg)
  (and (>= arg -1)
       (<= arg 543)))

(deftype pen-color ()
  `(and integer
	(satisfies match-pen-color-p)))

(defun match-area-fill-p (arg)
  (and (>= arg -1)
       (<= arg 62)))

(deftype area-fill ()
  `(and integer
	(satisfies match-area-fill-p)))

(defun match-area-direction-p (arg)
  (member arg '(0 1)))

(deftype arc-direction ()
  `(anf integer
	(satisfies match-area-direction-p)))

(defun match-join-style-p (arg)
  (and (>= arg 0)
       (<= arg 2)))

(deftype join-style ()
  `(and integer
	(satisfies match-join-style-p)))

(defun arrow-def1 (arg)
  (or (null arg)
      (typep arg 'arrow-def)))

(deftype arrow-def1 ()
  `(satisfies arrow-def1))