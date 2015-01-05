

(in-package :cl-user)
(defpackage :protolang.reader
  (:use :cl :cl-annot :cl-annot.class
        :protolang.errors
        :protolang.definition
        :yacc
        :cl-lex))

(in-package :protolang.reader)

(enable-annot-syntax)

