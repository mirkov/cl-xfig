(in-package :cl-xfig)

(defvar *line-styles*
  '(:default -1
   :solid 0
   :dashed 1
   :dotted 2
   :dash-dotted 3
   :dash-double-dotted 4
   :dash-triple-dotted 5)
  "Available line styles")

(defparameter *line-style* (getf *line-styles* :solid))
(defparameter *style-val* 0.01 "style value for dashed and dotted lines")

(defvar *join-styles*
  '(:miter 0
   :round 1
   :bevel 2))
(defparameter *join-style* (getf *join-styles* :round))

(defvar *cap-styles*
  '(:butt 0
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
(defvar *arrow-types*
  '(:stick 0
   :closed-triangle 1
   :indented-butt 2
   :pointed-butt 3))
(defparameter *arrow-type* :stick)

(defvar *arrow-styles*
  '(:hollow 0
    :filled 1))
(defparameter *arrow-style* :hollow)

(defparameter *arrow-thickness* 10)
(defparameter *arrow-width* 40)
(defparameter *arrow-height* 80)

(defvar *arrow-fill-styles*
  '(:hollow 0 ;; filled with white
   :filled 1))

;; color definitions
(defconstant +default-color+ -1)
(defconstant +black+ 0)
(defconstant +blue+ 1)
(defconstant +green+ 2)
(defconstant +cyan+ 3)
(defconstant +red+ 4)
(defconstant +magenta+ 5)
(defconstant +yellow+ 6)
(defconstant +white+ 7)
(defconstant +blue-1+ 8)
(defconstant +blue-2+ 9)
(defconstant +blue-3+ 10)
(defconstant +blue-4+ 11)
(defconstant +green-1+ 12)
(defconstant +green-3+ 13)
(defconstant +green-2+ 14)
(defconstant +cyan-1+ 15)
(defconstant +cyan-2+ 16)
(defconstant +cyan-3+ 17)
(defconstant +red-1+ 18)
(defconstant +red-2+ 19)
(defconstant +red-3+ 20)
(defconstant +magenta-1+ 21)
(defconstant +magenta-2+ 22)
(defconstant +magenta-3+ 23)
(defconstant +brown-1+ 24)
(defconstant +brown-2+ 25)
(defconstant +brown-3+ 26)
(defconstant +pink-1+ 27)
(defconstant +pink-2+ 28)
(defconstant +pink-3+ 29)
(defconstant +pink-4+ 30)
(defconstant +gold+ 31)

(defvar *colors*
  '((cons 'default-color -1)
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
(defconstant +gray-1+ "#cccccc")
(defconstant +gray-2+ "#999999")
(defconstant +gray-3+ "#666666")
(defconstant +gray-4+ "#333333")


;; default drawing style
(defparameter *thickness* 1)
(defparameter *pen-color* +black+)
(defparameter *fill-color* 7)
(defparameter *depth* 50)
(defparameter *pen-style* -1 "Not used by xfig")
(defparameter *area-fill* -1)
(defparameter *radius* -1)
(defparameter *forward-arrow* nil)
(defparameter *backward-arrow* nil)


(defvar *font-flags*
  '(:rigid 0
    :latex 1
    :ps 2
    :hidden 3)
  "Bits for text objects")

(defvar *font-field/latex*
  '(:default 0
    :roman 1
    :bold 2
    :italic 3
    :san-serif 4
    :typewriter 5)
  "`font' field values for the text object when bit 2 of
`font-flags' bit is set")

(defvar *font-field/ps*
  '(:Default-font -1
    :Times-Roman 0
    :Times-Italic 1
    :Times-Bold 2
    :Times-Bold-Italic 3
    :AvantGarde-Book 4
    :AvantGarde-Book-Oblique 5
    :AvantGarde-Demi 6
    :AvantGarde-Demi-Oblique 7
    :Bookman-Light 8
    :Bookman-Light-Italic 9
    :Bookman-Demi 10
    :Bookman-Demi-Italic 11
    :Courier 12
    :Courier-Oblique 13
    :Courier-Bold 14
    :Courier-Bold-Oblique 15
    :Helvetica 16
    :Helvetica-Oblique 17
    :Helvetica-Bold 18
    :Helvetica-Bold-Oblique 19
    :Helvetica-Narrow 20
    :Helvetica-Narrow-Oblique 21
    :Helvetica-Narrow-Bold 22
    :Helvetica-Narrow-Bold-Oblique 23
    :New-Century-Schoolbook-Roman 24
    :New-Century-Schoolbook-Italic 25
    :New-Century-Schoolbook-Bold 26
    :New-Century-Schoolbook-Bold-Italic 27
    :Palatino-Roman 28
    :Palatino-Italic 29
    :Palatino-Bold 30
    :Palatino-Bold-Italic 31
    :Symbol 32
    :Zapf-Chancery-Medium-Italic 33
    :Zapf-Dingbats   34)
  "`font' field values for post script fonts (when bit 2 of font-flags is set)")
