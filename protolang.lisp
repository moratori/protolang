
(defstruct ($tundef (:constructor $tundef (ident))
                    (:conc-name $tundef.)
                    (:print-object
                      (lambda (obj stream)
                        (format stream "~A" ($tundef.ident obj)))))
  ident)

(defstruct ($tint (:constructor $tint)
                  (:print-object 
                    (lambda (obj stream)
                      (declare (ignore obj))
                      (format stream "Integer")))))

(defstruct ($tbool (:constructor $tbool)
                   (:print-object 
                    (lambda (obj stream)
                      (declare (ignore obj))
                      (format stream "Boolean")))))

(defstruct ($tfunc (:constructor $tfunc (domain range))
                   (:conc-name $tfunc.)
                   (:print-object 
                    (lambda (obj stream)
                      (format stream "(~A -> ~A)"
                              (print-object ($tfunc.domain obj) nil)
                              (print-object ($tfunc.range obj) nil)))))
  (domain nil :type (or $tint $tbool $tfunc $tundef))
  (range nil  :type (or $tint $tbool $tfunc $tundef)))


(defstruct ($integer (:constructor $integer (value)))
  (value "" :type string))

(defstruct ($boolean (:constructor $boolean (value)))
  (value "" :type string))

(defstruct ($var (:constructor $var (value))
                 (:conc-name $var.))
  (value "" :type string))

(defstruct ($typedvar (:constructor $typedvar (var type))
                      (:conc-name $typedvar.))
  (var nil :type $var)
  (type nil :type (or null $tint $tbool $tundef $tfunc)))


(defstruct ($call (:constructor $call (ident exprs))
                  (:conc-name $call.))
  (ident "" :type string)
  (exprs nil :type list))

(defstruct ($special (:constructor $special (ident exprs))
                     (:conc-name $special.))
  (ident "" :type string)
  (exprs nil :type list))

(defstruct ($fn (:constructor $fn (arguments rtype body))
                (:conc-name $fn.))
  "arguments is list of $typedvar"
  (arguments nil :type list)
  (rtype nil :type (or null $tint $tbool $tfunc $tundef))
  (body nil :type (or $call $special $fn $integer $boolean $var)))

(defstruct ($def (:constructor $def (name fn)))
  name fn)


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


(defun lookup (key table)
  (assert (typep key 'string))
  (cdr (assoc key table :test #'string=)))


(defgeneric type= (a b)
  (:documentation
    "型オブジェクトが等しいか判定する
     型変数に関してはidentが等しいかをチェックする
     $tundef型のα と$tundef型のβ は等しくならない")
  (:method ((a t) (b t)) 
   (typep b (type-of a)))
  (:method ((a $tfunc) (b $tfunc))
   (and (type= ($tfunc.domain a) ($tfunc.domain b))
        (type= ($tfunc.range a) ($tfunc.range b))))
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
      (reduce 
        (lambda (env arg)
          (multiple-value-bind (type new) 
            (typecheck arg env)
            (setf args-type (nconc args-type (list type))) 
            new))
        args
        :initial-value env)
      args-type)))


(defun update-env (env old new)
  "env : ((x . TYPE-1) ...) について
   (substype TYPE-n old new) した新しい環境を返す"
  (mapcar 
    (lambda (x)
      (destructuring-bind (v . ty) x
        (cons v (substype ty old new)))) 
    env))

(defun unify-type-with-rule (type rule)
  "型type を rule: ((α  . β ) ...) で書き換える
   rule のドット対はいずれも型オブジェクト"
  (reduce 
    (lambda (type each)
      (destructuring-bind (old . new) each
        (substype type old new)))
    rule
    :initial-value type))


(defun unify-env-with-rule (env rule)
  "env: ((x . α ) ...) を
   rule: ((α  . β ) ...) で書き換えた新しい envを返す
   rule のドット対はいずれも型オブジェクト"
  (reduce 
    (lambda (env each)
      (destructuring-bind (old . new) each
        (update-env env old new)))
    rule
    :initial-value env))


(defun unify-env-with-env (env1 env2)
  "env1: ((x . α ) ...)
   env2: ((x . Int)...)
   を ((x . Int)...) のようにして返す。
   env1では不確定だった型変数を env2 を使って書き換える
   env2は一種の書き換え規則のように振る舞う"
  (mapcar 
    (lambda (e1)
      (reduce 
        (lambda (pair rule)
          (destructuring-bind (tl . tr) pair
            (destructuring-bind (rl . rr) rule
              (if (and (string= tl rl) (typep tr '$tundef))
                (cons rl rr)
                pair))))
        env2
        :initial-value e1))
    env1))


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
   (list (cons type1 type2)))
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
       (multiple-value-bind (new-env args-type)
         (arg-typecheck ($special.exprs obj) env)
         (destructuring-bind (contype thentype elsetype)
           args-type

           (unless 
             (type= contype ($tbool))
             (error "if: condition type is boolean"))
           
           (let ((rule (match thentype elsetype)))
             (values
               (unify-type-with-rule thentype rule)
               (unify-env-with-rule new-env rule)))))))))


(defmethod typecheck ((obj $call) env)
  (let ((ftype (lookup ($call.ident obj) 
                       (append env *primitive-function-type*)))
        (args ($call.exprs obj)))

    (unless (typep ftype '$tfunc)
      (error "function type required for function call: ~A" ftype)) 

    (multiple-value-bind (now-env args-type) 
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


(defun make-env (arguments)
  " lambda 式の仮引数から環境をつくる
    型が指定されていない場合は一意な型変数を割り当てる"
  (mapcar 
    (lambda (x)
      (let ((var ($typedvar.var x))
            (type ($typedvar.type x)))
        (cons ($var.value var)
              (if (null type) 
                ($tundef (symbol-name (gensym "tv")))
                type))))
    arguments))

(defun make-function-type (argenv rtype)
  (if (null argenv) rtype
    (destructuring-bind (_ . type) (car argenv)
      (declare (ignore _))
      ($tfunc type (make-function-type (cdr argenv) rtype)))))

(defmethod typecheck ((obj $fn) init-env)
  (let* ((argenv  (make-env ($fn.arguments obj)))
         (env (append argenv init-env))
         (typedresult ($fn.rtype obj))
         (expr ($fn.body obj)))

    (when (null argenv)
      (error "can't make constant function"))

    (multiple-value-bind (exprtype new-env)
      #|
      | new-env には　argenv 作成時には不明だった変数の型が推論された
      | 結果を含みうる
      |#
      (typecheck expr env)

      (unless (or (null typedresult) (type= typedresult exprtype))
        (error "type inconsistency: declare = ~A , but inferenced = ~A" 
               typedresult exprtype))

      (values 
        (make-function-type 
          (unify-env-with-env argenv new-env)
          exprtype)
        init-env))))


(defun typecheck-toplevel (obj)
  (typecheck obj nil))








