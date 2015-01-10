

(in-package :cl-user)
(defpackage :protolang.typecheck
  (:use :cl
        :protolang.errors
        :protolang.definition
        :cl-annot))
(in-package :protolang.typecheck)

(enable-annot-syntax)

(defun lookup (key table)
  (assert (typep key 'string))
  (cdr (assoc key table :test #'string=)))


@export
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


@export
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
      (error 
        (make-condition 'typecheck-error
           :message "unbound variable found. type unknown"
           :value  obj)))
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
   (error 
     (make-condition 'typecheck-internal-error
        :message "typeobject required. unexpected object"
        :value (list type1 type2))))
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
         (destructuring-bind (contype thentype elsetype) args-type

           (unless (or (typep contype '$tundef) (type= contype ($tbool)))
             (error 
               (make-condition 'typecheck-error
                  :message "special form if: first argument is boolean"               
                  :value contype)))
 
           (let ((new-env 
                   (if (typep contype '$tundef) 
                     (update-env new-env contype ($tbool))
                     new-env))
                 ;; contype が 未定の場合は new-env 中のcontypeを bool にした
                 ;; 新しい型環境を new-env としている
                 (rule (match thentype elsetype)))
             (values
               (unify-type-with-rule thentype rule)
               (unify-env-with-rule new-env rule)))))))))



(defun inference-function-type (args-type)
  "関数の型を引数から推論する
   x(1,2,bool) みたいな呼ばれ方をしていたら
   Int -> (Int -> (Bool -> α ))を返す
   ARG-TYPE1 -> (ARG-TYPE2 -> (ARG-TYPE3 -> (...)))
   この関数はmake-function-type に酷似しているのでよろしくない"
  (if (null args-type)
    ($tundef)
    ($tfunc (car args-type) (inference-function-type (cdr args-type)))))


(defun lookup-function-type% (ident ftype args-type env)
  "lookup-function-typeのヘルパ関数
   求めた関数型が型変数であったばあいに、引数の型から
   関数型をもとめる"
  (if (typep ftype '$tundef)
    (let ((infered (inference-function-type args-type)))
      (if (typep ident 'string)
        (values infered args-type (update-env env ftype infered))
        (values infered args-type env)))
    (values ftype args-type env)))


(defun lookup-function-type (obj env)
  "呼び出される関数の型を求める
   関数の型 , 関数の引数の型 , 型環境を求める"
  (multiple-value-bind (env args-type)
    (arg-typecheck ($call.exprs obj) env)
    (let ((ident ($call.ident obj)))
      (etypecase ident 
        (string 
          (lookup-function-type%
            ident
            (lookup ident (append env *primitive-function-type*))
            args-type
            env))
        ($var 
          (lookup-function-type%
            ($var.value ident)
            (lookup ($var.value ident) (append env *primitive-function-type*))
            args-type
            env))
        ($fn
          (multiple-value-bind (type new-env)
            (typecheck ident env)
            (lookup-function-type%
              ident type args-type new-env)))))))
 

(defmethod typecheck ((obj $call) env)
  (multiple-value-bind (ftype args-type now-env)
    (lookup-function-type obj env) 
    
    (unless (typep ftype '$tfunc)
      (error 
        (make-condition 'typecheck-error
          :message "function type required for function call"              
          :value obj)))
    #|
    | 以下の reduce で関数呼び出しが行われた後の型を求める
      ftype : Int -> Int  , arg : Int  => Int
      ftype : Int -> Int  , arg : α    => Int (ただし型環境を更新[Int/α])
      ftype : α   -> α    , arg : Int  => Int 
    |# 
    (values
      (reduce 
        (lambda (rtype at) 
          (let ((domain ($tfunc.domain rtype)))
            (cond 
              ((type= domain at)
               ($tfunc.range rtype))
              ((typep domain '$tundef)
               (substype ($tfunc.range rtype) domain at))
              ((typep at '$tundef)
               (setf now-env (update-env now-env at domain))
               ($tfunc.range rtype))
              (t 
               (error 
                 (make-condition 'typecheck-error
                   :message "invalid function call"
                   :value (list  ftype (list domain at))))))))
        args-type
        :initial-value ftype)
      now-env)))


(defun make-env (arguments)
  "lambda 式の仮引数から環境をつくる
    型が指定されていない場合は一意な型変数を割り当てる"
  (mapcar 
    (lambda (x)
      (let ((var ($typedvar.var x))
            (type ($typedvar.type x)))
        (cons ($var.value var)
              (if (null type) 
                ($tundef)
                type))))
    arguments))

(defun make-function-type (argenv rtype)
  (if (null argenv) rtype
    (destructuring-bind (_ . type) (car argenv)
      (declare (ignore _))
      ($tfunc type (make-function-type (cdr argenv) rtype)))))


(defun remove-local-type-bound (new-env argenv)
  "arg-envで導入された型環境の情報をnew-envから取り除く
   [x] -> 
     [y] -> y + 1
   y が Integerであると推論された情報は上位では必要ない
   ((x . t1)) 
   ((y . t2) (x . t1))
   ((y . Integer) (x . t1))"
  (reduce 
    (lambda (target-env local)
      (destructuring-bind (var . type) local
       (declare (ignore type)) 
       (loop with flag = nil
             for (nvar . ntype) in target-env
             if (string= nvar var) 
               do (setf flag t)
             if (and (String= nvar var) (not flag)) 
               collect (cons nvar ntype))))
    argenv
    :initial-value new-env))

(defmethod typecheck ((obj $fn) init-env)
  (let* ((argenv  (make-env ($fn.arguments obj)))
         (env (append argenv init-env))
         (typedresult ($fn.rtype obj))
         (expr ($fn.body obj)))

    (when (null argenv)
      (error
        (make-condition 'typecheck-error
          :message "can't make constant function")))

    (multiple-value-bind (exprtype new-env)
      #|
      | new-env には　argenv 作成時には不明だった変数の型が推論された
      | 結果を含みうる
      |#
      (typecheck expr env)

      (unless (or (null typedresult) (type= typedresult exprtype))
        (error 
          (make-condition 'typecheck-error
            :message "type inconsistency found: does not match declared type and inferenced type"
            :value (list typedresult exprtype))))

      (values 
        (make-function-type 
          (unify-env-with-env argenv new-env)
          exprtype)
        (remove-local-type-bound new-env argenv)))))

(defmethod typecheck ((obj $def) env)
  (let* ((name ($def.name obj))
         (fn   ($def.fn obj))
         (rtype($fn.rtype fn)))
    (if rtype 
      (multiple-value-bind (type new-env)
        (typecheck fn (acons name ($tfunc ($tundef) rtype) env))
        (values 
          type 
          (acons name type 
                 (remove-if 
                   (lambda (x) (string= x name))
                   new-env
                   :key #'car))))
      (multiple-value-bind (type new-env)
        (typecheck fn env)
        (values 
          type 
          (acons name type new-env))))))

