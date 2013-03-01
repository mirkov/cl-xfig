(asdf:defsystem :cl-xfig
    :name "cl-xfig"
    :author "Mirko Vukovic mirko.vukovic@us.tel.com"
    :version "0.1"
    :maintainer "Mirko Vukovic mirko.vukovic@us.tel.com"
    :licence "TEL"
    :description "xfig generation from cl"
    :long-description ""
    :serial t
    :depends-on
    ("lisp-unit"
     "alexandria"
     "anaphora")
    :components
    ((:file "cl-xfig-package-def")
     (:file "page-definition")
     (:file "drawing-style-variables")
     (:file "types")
     (:file "figure-objects")
     (:file "object-generation")
     (:file "emit-objects")
     (:file "samples")))
    