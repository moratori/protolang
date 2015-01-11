

(in-package :cl-user)
(defpackage :protolang.definition
  (:use :cl :cl-annot :cl-annot.class
        :protolang.errors
        ))
(in-package :protolang.definition)

(enable-annot-syntax)

@export-structure
(defstruct ($tundef (:constructor $tundef ())
                    (:conc-name $tundef.)
                    (:print-object
                      (lambda (obj stream)
                        (format stream "~A" ($tundef.ident obj)))))
  (ident (symbol-name (gensym "tv"))))

@export-structure
(defstruct $tliteral)

@export-structure
(defstruct ($tint (:constructor $tint)
                  (:include $tliteral)
                  (:print-object 
                    (lambda (obj stream)
                      (declare (ignore obj))
                      (format stream "Integer")))))
@export-structure
(defstruct ($tbool (:constructor $tbool)
                   (:include $tliteral)
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
(defstruct ($integer (:constructor $integer (value))
                     (:conc-name $integer.)
                     (:print-object 
                       (lambda (obj stream)
                         (format stream "~A" ($integer.value obj)))))
  (value "" :type string))

@export-structure
(defstruct ($boolean (:constructor $boolean (value))
                     (:conc-name $boolean.)
                     (:print-object 
                       (lambda (obj stream)
                         (format stream "~A" ($boolean.value obj)))))
  (value "" :type string))

@export-structure
(defstruct ($var (:constructor $var (value))
                 (:conc-name $var.)
                 (:print-object
                   (lambda (obj stream)
                     (format stream "~A" ($var.value obj)))))
  (value "" :type string))

@export-structure
(defstruct ($typedvar (:constructor $typedvar (var type))
                      (:conc-name $typedvar.)
                      (:print-object 
                        (lambda (obj stream)
                          (format stream "~A" ($typedvar.var obj))
                          (when ($typedvar.type obj)
                            (format stream " : ~A" ($typedvar.type obj))))))
  (var nil :type $var)
  (type nil :type (or null $tint $tbool $tundef $tfunc)))


@export-structure
(defstruct ($call (:constructor $call (ident exprs))
                  (:conc-name $call.)
                  (:print-object 
                    (lambda (obj stream)
                      (format stream "~A[~{~A~^,~}]"
                              ($call.ident obj)
                              ($call.exprs obj)))))
  (ident nil)
  (exprs nil :type list))

@export-structure
(defstruct ($special (:constructor $special (ident exprs))
                     (:conc-name $special.)
                     (:print-object 
                       (lambda (obj stream)
                         (format stream "~A ~{~A~^ ~}"
                                 ($special.ident obj)
                                 ($special.exprs obj)))))
  (ident "" :type string)
  (exprs nil :type list))

@export-structure
(defstruct ($fn (:constructor $fn (arguments rtype body))
                (:conc-name $fn.)
                (:print-object
                  (lambda (obj stream)
                    (format stream "[~{~A~^,~}]"
                            ($fn.arguments obj))
                    (when ($fn.rtype obj)
                      (format stream ":~A" ($fn.rtype obj)))
                    (format stream " -> ~A" ($fn.body obj)))))
  (arguments nil :type list)
  (rtype nil :type (or null $tint $tbool $tfunc $tundef))
  (body nil :type (or $call $special $fn $integer $boolean $var)))

@export-structure
(defstruct ($def (:constructor $def (name fn))
                 (:conc-name $def.)
                 (:print-object 
                   (lambda (obj stream)
                     (format stream "def ~A" ($def.name obj))
                     (format stream "~t~A" ($def.fn obj)))))
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


(defpackage :plang-user
  (:use :cl :cl-annot))
(in-package :plang-user)

(enable-annot-syntax)

(intern "MAIN" :plang-user)

@export
(defvar *primitive-function*
  (list 
    (cons "+"   `(lambda (x) (lambda (y) (+ x y))))
    (cons "-"   `(lambda (x) (lambda (y) (- x y))))
    (cons "*"   `(lambda (x) (lambda (y) (* x y))))
    (cons "%"   `(lambda (x) (lambda (y) (mod x y))))
    (cons "<"   `(lambda (x) (lambda (y) (< x y))))
    (cons "=="  `(lambda (x) (lambda (y) (= x y))))
    (cons "&&"  `(lambda (x) (lambda (y) (and x y))))
    (cons "||"  `(lambda (x) (lambda (y) (or x y))))
    (cons "!"   `(lambda (x) (not x))))
  "組み込みで用意する関数の実体")
 

