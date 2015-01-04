

(in-package :cl-user)
(defpackage :prorolang.compiler 
  (:use :cl :cl-annot :cl-annot.class
        :protolang.errors
        :protolang.definition
        :protolang.typecheck))

(in-package :prorolang.compiler)

(enable-annot-syntax)
