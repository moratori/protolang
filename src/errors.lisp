

(in-package :cl-user)
(defpackage :protolang.errors 
  (:use :cl :cl-annot :cl-annot.class)
  (:export 
    :protolang-compile-error
    :message
    )
  )
(in-package :protolang.errors)


(define-condition protolang-compile-error (error) 
  ((message 
     :initform ""
     :initarg :message
     :reader message)))
