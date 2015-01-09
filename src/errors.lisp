

(in-package :cl-user)
(defpackage :protolang.errors 
  (:use :cl :cl-annot :cl-annot.class)
  (:export 
    :message
    :value
    )
  )
(in-package :protolang.errors)


(enable-annot-syntax)


@export
(define-condition protolang-compile-error (error) 
  ((message 
     :initform ""
     :initarg :message
     :reader message)
   (value
     :initform nil
     :initarg :value
     :type t
     :reader value)))

@export
(define-condition conversion-error (protolang-compile-error) ())
@export
(define-condition special-form (conversion-error) ())

@export
(define-condition typecheck-error (protolang-compile-error) ())
@export
(define-condition typecheck-internal-error (typecheck-error) ())
