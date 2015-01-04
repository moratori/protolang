

(in-package :cl-user)
(defpackage :protolang.grammer
  (:use :cl :cl-annot :cl-annot.class
        :protolang.errors
        :protolang.definition
        :yacc
        :cl-lex))

(in-package :protolang.grammer)
(enable-annot-syntax)
