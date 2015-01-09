

(in-package :cl-user)
(defpackage :protolang.compiler 
  (:use :cl :cl-annot :cl-annot.class
        :plang-user
        :protolang.errors
        :protolang.definition
        :protolang.typecheck))

(in-package :protolang.compiler)

(enable-annot-syntax)



@export
(defgeneric ->sexpr (obj)
  (:documentation 
    "中間言語(protolang.definitionで定義される構造体)
     をCommon Lisp 処理系で実行可能なS式に変換する")
  (:method ((obj $integer))
   `(the integer ,(parse-integer ($integer.value obj))))
  (:method ((obj $boolean))
   `(the boolean 
         ,(when (string= ($boolean.value obj) "true") 
           t)))
  (:method ((obj $var))
   (intern ($var.value obj) :plang-user))
  (:method ((obj $typedvar))
   (->sexpr ($typedvar.var obj))))


(defun lookup (key table)
  (assert (typep key 'string))
  (cdr (assoc key table :test #'string=)))

(defun inner-callee (ident)
  "関数呼び出しを適切なものに変換する"
  (if (stringp ident)
    (let ((obj (lookup ident *primitive-function*)))
      (if obj  obj
        (intern ident :plang-user)))
    (->sexpr ident)))


(defmethod ->sexpr ((obj $call))
  (let*  ((ident ($call.ident obj))
          (exprs ($call.exprs obj))
          (callee (inner-callee ident)))
    (reduce 
      (lambda (r x)
        `(funcall ,r ,(->sexpr x)))
      exprs
      :initial-value callee)))


(defmethod ->sexpr ((obj $special))
  (let ((ident ($special.ident obj))
        (exprs (mapcar #'->sexpr ($special.exprs obj))))
    (cond 
      ((string= ident "if")
       (destructuring-bind (cond then else) 
         exprs
         `(if ,cond ,then ,else)))
      (t 
       (error 
         (make-condition 'special-form
            :message "unexpected special form"
            :value ident))))))


(defmethod ->sexpr ((obj $fn))
  (let ((body (->sexpr ($fn.body obj))))
    (reduce 
      (lambda (r x)
        `(lambda (,(->sexpr x))
           ,r))
      (reverse ($fn.arguments obj))
      :initial-value body)))


(defmethod ->sexpr ((obj $def))
  (let* ((name ($def.name obj))
         (sym  (intern name :plang-user))
         (body (->sexpr ($def.fn obj))))
    `(defvar ,sym 
       (let (,sym)
               (setf ,sym ,body)
               ,sym))))

@export
(defun ->sexpr-toplevel (objects)
  "抽象表現のリストobjectsをとってそれを
   S式に変換するトップレベルの関数
   各式についてtypecheckも行う"
  (cons 'progn
        (loop 
          with env = nil
          for expr in objects
          for (type new-env) = (multiple-value-list (typecheck expr env))
          do (setf env new-env)
          collect (->sexpr expr))))






