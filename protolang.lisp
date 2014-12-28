
(defstruct ($tundef (:constructor $tundef (ident))
                    (:conc-name $tundef.))
  ident)
(defstruct ($tint (:constructor $tint)))
(defstruct ($tbool(:constructor $tbool)))
(defstruct ($tfunc (:constructor $tfunc (domain range)))
  (domain nil :type (or $tint $tbool $tfunc))
  (range nil  :type (or $tint $tbool $tfunc)))


(defstruct ($integer (:constructor $integer (value)))
  (value "" :type string))

(defstruct ($boolean (:constructor $boolean (value)))
  (value "" :type string))

(defstruct ($var (:constructor $var (value))
                 (:conc-name $var.))
  (value "" :type string))


(defstruct ($call (:constructor $call (ident exprs))
                  (:conc-name $call.))
  (ident "" :type string)
  (exprs nil :type list))

(defstruct ($special (:constructor $special (ident exprs))
                     (:conc-name $special.))
  (ident "" :type string)
  (exprs nil :type list))

(defstruct ($fn (:constructor $fn (argument rtype body)))
  (argument nil :type list)
  (rtype nil :type (or nil $tint $tbool $tfunc))
  (body nil :type (or $call $special $fn)))

(defstruct ($def (:constructor $def (name fn)))
  name fn)


(defvar *primitive-function-type*
  (list 
    (list "+"   ($tfunc ($tint)  ($tfunc ($tint) ($tint))))
    (list "-"   ($tfunc ($tint)  ($tfunc ($tint) ($tint))))
    (list "*"   ($tfunc ($tint)  ($tfunc ($tint) ($tint))))
    (list "%"   ($tfunc ($tint)  ($tfunc ($tint) ($tint)) ))
    (list "<"   ($tfunc ($tbool) ($tfunc ($tint) ($tint))))
    (list "=="  ($tfunc ($tbool) ($tfunc ($tint) ($tint)) ))
    (list "&&"  ($tfunc ($tbool) ($tfunc ($tbool) ($tbool)) ))
    (list "||"  ($tfunc ($tbool) ($tfunc ($tbool) ($tbool)) )))
  "組み込みで用意する関数の型の定義")


(defun lookup (key table)
  (cdr (assoc key table :test #'string=)))


(defgeneric type= (a b)
  (:documentation
    "型オブジェクトが等しいか判定する
     型変数に関してはidentが等しいかをチェックする")
  (:method ((a t) (b t)) 
   (typep b (type-of a)))
  (:method ((a $tundef) (b $tundef))
   (equal ($tundef.ident a) ($tundef.ident b))))


(defgeneric typecheck (obj env)
  (:documentation 
    "obj が整合的に型付けされるかを判定して 
     (つまり型検査 (型が明示されていない場合は推論))
     そのobjの型と 新しい型環境を返す")
  (:method ((obj $integer) env) 
   (values ($tint)  env))
  (:method ((obj $boolean) env) 
   (values ($tbool) env)))


(defmethod typecheck ((obj $var) env)
  (let ((res (lookup ($var.value obj) env)))
    (when (null res)
      (error "uninitialized variable"))
    (values res env)))


(defmethod typecheck ((obj $call) env)
  (let ((ftype (lookup ($call.ident obj) 
                       (append env *primitive-function-type*)))
        (args ($call.exprs obj))
        (args-type nil))

    (unless (typep ftype '$tfunc)
      (error "function type required")) 

    (let ((now-env 
            (reduce 
              (lambda (env arg)
                (multiple-value-bind (type new) 
                  (typecheck arg env)
                  (setf args-type (nconc args-type (list type)))
                  new))
              args
              :initial-value env)))
      
      (reduce 
        (lambda (rtype at) 
          (let ((domain ($tfunc.domain rtype)))
            #|
            | at が α  で domain が Int とかだったら
            | Int/α  の書き換え規則で now-env を書き換えないといけない
              at が α　型であることしか意図してないけど
              rtype (関数型) だって α ->α  みたいな場合もありうるけどもそれは意図していない
            |#
            (cond 
              ((type= domain at)
               ($tfunc.range rtype))
              (t )
              )
            )
          )
        args-type
        :initial-value ftype
        )
      )  
    )
  )












