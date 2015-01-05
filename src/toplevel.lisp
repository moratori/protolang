

(in-package :cl-user)
(defpackage :protolang
  (:use :cl
        :protolang.errors
        :protolang.definition
        :protolang.reader
        :protolang.compiler))

(in-package :protolang)
