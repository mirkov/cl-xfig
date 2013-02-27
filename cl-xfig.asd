(defpackage :cl-xfig-asd
  (:use :asdf :cl))
(in-package :cl-xfig-asd)

(defsystem :cl-xfig
    :name "cl-xfig"
    :author "Mirko Vukovic mirko.vukovic@us.tel.com"
    :version "0.1"
    :maintainer "Mirko Vukovic mirko.vukovic@us.tel.com"
    :licence "TEL"
    :description "xfig generation from cl"
    :long-description ""
    :components
    ((:file "cl-xfig")))
    