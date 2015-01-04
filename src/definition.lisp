

(in-package :cl-user)
(defpackage :protolang.definition
  (:use :cl :cl-annot :cl-annot.class
        :protolang.errors
        ))
(in-package :protolang.definition)

(enable-annot-syntax)

@export-structure
(defstruct ($tundef (:constructor $tundef (ident))
                    (:conc-name $tundef.)
                    (:print-object
                      (lambda (obj stream)
                        (format stream "~A" ($tundef.ident obj)))))
  ident)

@export-structure
(defstruct ($tint (:constructor $tint)
                  (:print-object 
                    (lambda (obj stream)
                      (declare (ignore obj))
                      (format stream "Integer")))))
@export-structure
(defstruct ($tbool (:constructor $tbool)
                   (:print-object 
                    (lambda (obj stream)
                      (declare (ignore obj))
                      (format stream "Boolean")))))

@export-structure
(defstruct ($tfunc (:constructor $tfunc (domain range))
                   (:conc-name $tfunc.)
                   (:print-object 
                    (lambda (obj stream)
                      (format stream "(~A -> ~A)"
                              (print-object ($tfunc.domain obj) nil)
                              (print-object ($tfunc.range obj) nil)))))
  (domain nil :type (or $tint $tbool $tfunc $tundef))
  (range nil  :type (or $tint $tbool $tfunc $tundef)))


@export-structure
(defstruct ($integer (:constructor $integer (value)))
  (value "" :type string))

@export-structure
(defstruct ($boolean (:constructor $boolean (value)))
  (value "" :type string))

@export-structure
(defstruct ($var (:constructor $var (value))
                 (:conc-name $var.))
  (value "" :type string))

@export-structure
(defstruct ($typedvar (:constructor $typedvar (var type))
                      (:conc-name $typedvar.))
  (var nil :type $var)
  (type nil :type (or null $tint $tbool $tundef $tfunc)))


@export-structure
(defstruct ($call (:constructor $call (ident exprs))
                  (:conc-name $call.))
  (ident nil)
  (exprs nil :type list))

@export-structure
(defstruct ($special (:constructor $special (ident exprs))
                     (:conc-name $special.))
  (ident "" :type string)
  (exprs nil :type list))

@export-structure
(defstruct ($fn (:constructor $fn (arguments rtype body))
                (:conc-name $fn.))
  (arguments nil :type list)
  (rtype nil :type (or null $tint $tbool $tfunc $tundef))
  (body nil :type (or $call $special $fn $integer $boolean $var)))

@export-structure
(defstruct ($def (:constructor $def (name fn)))
  name fn)


@export
(defvar *primitive-function-type*
  (list 
    (cons "+"   ($tfunc ($tint)  ($tfunc ($tint) ($tint))))
    (cons "-"   ($tfunc ($tint)  ($tfunc ($tint) ($tint))))
    (cons "*"   ($tfunc ($tint)  ($tfunc ($tint) ($tint))))
    (cons "%"   ($tfunc ($tint)  ($tfunc ($tint) ($tint)) ))
    (cons "<"   ($tfunc ($tint)  ($tfunc ($tint) ($tbool))))
    (cons "=="  ($tfunc ($tint)  ($tfunc ($tint) ($tbool)) ))
    (cons "&&"  ($tfunc ($tbool) ($tfunc ($tbool) ($tbool)) ))
    (cons "||"  ($tfunc ($tbool) ($tfunc ($tbool) ($tbool)) ))
    (cons "!"   ($tfunc ($tbool) ($tbool))))
  "組み込みで用意する関数の型の定義")








