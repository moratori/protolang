
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
     型変数に関してはidentが等しいかをチェックする
     $tundef型のα と$tundef型のβ は等しくならない")
  (:method ((a t) (b t)) 
   (typep b (type-of a)))
  (:method ((a $tundef) (b $tundef))
   (equal ($tundef.ident a) ($tundef.ident b))))


(defgeneric substype (target old new)
  (:documentation 
    "target 中の old を new にした新しい target' を返す
     oldとtype=となるものをnewにおきかえる")
  (:method ((target $tint) old new)
   " $tint が置き換え元となる事はない"
   target)
  (:method ((target $tbool) old new)
   "$tbool が置き換え元になることはない"
   target))


(defmethod substype ((target $tundef) (old $tundef) new)
  (if (type= target old) new target))

(defmethod substype ((target $tfunc) (old $tundef) new)
  ($tfunc 
    (substype ($tfunc.domain target) old new)
    (substype ($tfunc.range target) old new)))


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


(defun arg-typecheck (args env)
   "関数呼び出しの引数のリスト args を
    型環境 env で 判定し、その型のリスト
    と新しい型環境を返す"
  (let ((args-type nil))
    (values 
      args-type
      (reduce 
        (lambda (env arg)
          (multiple-value-bind (type new) 
            (typecheck arg env)
            (setf args-type (nconc args-type (list type))) 
            new))
        args
        :initial-value env))))


(defun update-env (env old new)
  "env : ((x . TYPE-1) ...) について
   (substype TYPE-n old new) した新しい環境を返す"
  (mapcar 
    (lambda (x)
      (destructuring-bind (v . ty) x
        (list v (substype ty old new)))) 
    env))

(defun unify-type (type rule)
  "型type を rule: ((α  . β ) ...) で書き換える"
  (reduce 
    (lambda (type each)
      (destructuring-bind (old . new) each
        (substype type old new)))
    rule
    :initial-value type))


(defun unify-env (env rule)
  "env: ((x . α ) ...) を
   rule: ((α  . β ) ...) で書き換えた新しい envを返す"
  (reduce 
    (lambda (env each)
      (destructuring-bind (old . new) each
        (update-env env old new)))
    rule
    :initial-value env))


(defgeneric match (type1 type2) 
  (:documentation
    "２つの型type1 type2が等しくなるような単一化子を求める
     (($tundef . new) ...)

     なぜ必要となるか
     if式は1つのbooleanを返す式と、型の等しい式exp1,exp2をとる
     1つ目の式が boolean を返すか否かは簡単に判別可能
     exp1とexp2は共に等しい型でなければいけない
     exp1,exp2の型がそれぞれ関数型でなければ簡単
     f = (x) => 1 : α ->Int
     g = (y) => 2 : β ->Int
     (f) =>
      (g) => if true f g
     みたいな時は f とgの型をそれぞれ再帰的にみて[α /β ]を見つけなければならない")

  (:method ((type1 t) (type2 t))
   (error "type unmateched"))
  (:method ((type1 $tint) (type2 $tint))
   nil)
  (:method ((type1 $tbool) (type2 $tbool))
   nil)
  (:method ((type1 $tundef) (type2 t))
   ;; ここで出現検査を行うべきかもしれない
   (list (list type1 type2)))
  (:method ((type1 t) (type2 $tundef))
   (match type2 type1)))


(defmethod match ((type1 $tfunc) (type2 $tfunc))
  (append 
    (match ($tfunc.domain type1) ($tfunc.domain type2))
    (match ($tfunc.range type1) ($tfunc.range type2))))



(defmethod typecheck ((obj $special) env)
  (let ((ident ($special.ident obj)))
    (cond 
      ((string= ident "if")
       (multiple-value-bind (args-type new-env)
         (arg-typecheck ($special.exprs obj) env)
         (destructuring-bind (contype thentype elsetype)
           args-type

           (unless 
             (type= contype ($tbool))
             (error "if: condition type is boolean"))
           
           (let ((rule (match thentype elsetype)))
             (values
               (unify-type thentype rule)
               (unify-env new-env rule)))))))))


(defmethod typecheck ((obj $call) env)
  (let ((ftype (lookup ($call.ident obj) 
                       (append env *primitive-function-type*)))
        (args ($call.exprs obj)))

    (unless (typep ftype '$tfunc)
      (error "function type required for function call")) 

    (multiple-value-bind (args-type now-env) 
      (arg-typecheck args env)
      
      #|
      | 以下の reduce で関数呼び出しが行われた後の型を求める
      | ftype : Int -> Int  , arg : Int  => Int
      | ftype : Int -> Int  , arg : α    => Int (ただし型環境を更新[Int/α])
      | ftype : α   -> α    , arg : Int  => Int 
      |#
      (values
       (reduce 
        (lambda (rtype at) 
          (let ((domain ($tfunc.domain rtype)))
            #|
            | at が α  で domain が Int とかだったら
            | Int/α  の書き換え規則で now-env を書き換えないといけない
              at が α　型であることしか意図してないけど
              rtype (関数型) が α ->α  みたいな場合もありうるけどもそれは意図していない
            |#
            (cond 
              ((type= domain at)
               ($tfunc.range rtype))
              ((typep domain '$tundef)
               (substype ($tfunc.range rtype) domain at))
              ((typep at '$tundef)
               (setf now-env (update-env now-env at domain))
               ($tfunc.range rtype))
              (t 
               (error "unexpected error during function call type checking")))))
        args-type
        :initial-value ftype)
       now-env))))


(defmethod typecheck ((obj $fn) env)
  
  )





