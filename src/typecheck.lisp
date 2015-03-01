

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
   (equal ($tundef.ident a) ($tundef.ident b)))

  (:method ((a $tuser) (b $tuser))
   (and 
     (string= ($tuser.ident a) ($tuser.ident b))
     (every #'type= ($tuser.args a) ($tuser.args b)))))



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


(defmethod substype ((target $tuser) (old $tundef) new)
  ($tuser 
    ($tuser.ident target)
    (mapcar 
      (lambda (x)
        (substype x old new)) 
      ($tuser.args target))))

(defmethod substype ((target $typecons) old new)
  ($typecons 
    ($typecons.ident target)
    (mapcar 
      (lambda (each)
        (substype each old new))
      ($typecons.args target))))


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
    と新しい型環境を返す
    例
    式の列 A1 A2 A3 と初期の型環境 envについて
    ac(A1,env) -> typeA1 E1
    ac(A2,E1)  -> typeA2 E2
    ac(A3,E2)  -> typeA3 E3
    各 typeAn と 最後の型環境E3を返す"
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

(defun arg-typecheck-toplevel (args env)
  "arg-typecheck で　最後の式の型推論を行った時の
    型環境がもっとも情報量が多い。
    初期値の型環境でA1の式の型が未定であったとしても
    A2,A3の式中で判明した情報 (型環境)を用いれば
    A1の型を知ることができる。
    関数はそれを繰り返し、新たに判明するものがなくなるまで繰り返す"
  (multiple-value-bind (init-env args-type)
    (arg-typecheck args env)
    (loop with old-args-type =  args-type
          with old-env = init-env 
          with old-num = (count-if (lambda (x) (typep x '$tundef)) old-args-type)
          finally (return-from arg-typecheck-toplevel (values old-env old-args-type))
          for (next-env next-args-type) = (multiple-value-list (arg-typecheck args old-env))
          for next-num = (count-if (lambda (x) (typep x '$tundef)) next-args-type)
          while (< next-num old-num)
          do 
           (setf old-args-type next-args-type
                 old-env next-env
                 old-num next-num))))


#|
  env と rule は似て非なるもの
  env はある変数(型変数ではなくプログラム中で使われる普通の変数)
  がどの型であるかの情報を保持しているリスト。
  かつ、そのalistのcarは変数オブジェクトではなく文字列型で保持されているので
  [(String,Type)]となるもの。
  一方 rule は型と型のmatchの結果生まれた、単一化子のこと
  例えば Type<a,Integer> Type<c,c> の場合 ((a . c) (c . Integer))となる
|#




(defun contradict-rulep (rule)
  "((a . B) (a . C)) のようなおかしな環境となっていないか
   (($tundef . Type1) ...) "
  (loop for (dom1 . range1) in rule do
    (loop for (dom2 . range2) in rule do
      (assert (and (typep dom1 '$tundef) 
                   (typep dom2 '$tundef)))
      (when (and (type= dom1 dom2)
                 (not (type= range1 range2)))
        (return-from contradict-rulep t)))))

(defun remove-id-rule (rule)
  "型環境から意味のないruleを除去する
   ((x . x))のような"
  (remove-if 
    (lambda (x)
      (type= (car x) (cdr x)))
    rule)) 


(defgeneric occur-type (a b)
  (:documentation 
    "型b中に型変数aが出現するかを判定する")
  (:method ((type1 t) (type2 t))
   (type= type1 type2))
  (:method ((type1 $tundef) (type2 $tuser))
   (some 
     (lambda (x)
       (occur-type type1 x))
     ($tuser.args type2))))


(defun rewrite-rule (rule left right)
  "書き換え規則ruleを一度走査して、その右辺に出現する
   leftをrightに書き換える"
  (mapcar 
    (lambda (a.b)
      (cons (car a.b) (substype (cdr a.b) left right)))
    rule))

(defun regulate-rule (rule)
  "rule を正規形にする.
   ここでいう正規系とは((a . C) (b . (T a)) (d . (U b)))
   のような rule を ((a . C) (b . (T C)) (d . (U (T C))))
   にすること.
   終了条件は全てのrule対の右辺に全てのrule対の左辺が出現しないようになること
   
   当然だけれども現状の出現検査をしない処理では無限ループに陥る場合がある
   たとえば ((a . b) (b . (T a))) みたいのをやろうとしたとき
   とにかく ((α  . (T ... α　...))) となるようなパターンはループする" 

  (labels
    ((occur-in-right (rule left)
       "何れかの書き換え規則ruleの右辺中にleftが出現することを確認する"
       (some 
         (lambda (a.b)
           (destructuring-bind (l . r) a.b
             (occur-type left r)))
         rule)))
    (loop 
      named exit
      with res = rule
      finally (return-from exit res)
      for (left . right) in rule
      while (occur-in-right res left)
      do (setf res (rewrite-rule res left right)))))


(defun simplify-rule (rule)
  "ruleを綺麗にする
   
   まず remove-id-rule によって ((x . x))のようなものを取り除く
   次に regulate-rule してrule右辺中に何れかの左辺が出現しないようにする
   これをtmpとする
   remove-duplicatesによって A: ((a . tv1) (a . tv2) (a . tv3)) のような
   左辺が同じで右辺が異なる型変数であるものをtmpから得る。
   それをつかって B: ((tv1 . tv2) (tv2 . tv3)) をつくり equityに束縛
   最後にtmpからAを除去したもにBをappendして返す
   "
  (let* ((tmp (regulate-rule (remove-id-rule rule)))
         (tundef-equal-pair 
           ;; ((a . tv1) (a . tv2) ...)
           ;; のようなリストを求める
          (remove-duplicates tmp
            :test 
            (lambda (a b)
              (destructuring-bind (a1 . a2) a
                (destructuring-bind (b1 . b2) b
                  (not (and (type= a1 b1) 
                           (typep a2 '$tundef) 
                           (typep b2 '$tundef)))))))))

    (let* ((equity
            (if (< (length tundef-equal-pair) 2) tmp 
             (cons (car tundef-equal-pair) 
              (loop
                named exit
                with acc = tundef-equal-pair
                for a = (first acc) then b
                for b = (second acc) 
                while (> (length acc) 1)
                collect
                (progn 
                  (setf acc (cdr acc))
                  (cons (cdr a) (cdr b)))))))
          (res 
            (append equity
                    (remove-if 
                      (lambda (a.b)
                        (some 
                          (lambda (c.d)
                            (and (type= (car a.b) (car c.d))
                                 (not (type= (cdr a.b) (cdr c.d)))))
                          tmp))
                      tmp))))
      (when (contradict-rulep res)
          (error "type unmatched to constructor schema: ~%~A" res))
      res)))


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
   env2は一種の書き換え規則のように振る舞う
   => 不確定性の高い状態にならないか？
      型変数の含む数などで順序つける必要はないか？
      => ある時点で型が分からないから型変数が導入されるのであって
         env1 に於いて型が判定しているものについて env1を継承しているenv2において
         型変数が導入されるはずはない"
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
        :message "unmatched typed object found"
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

(defmethod match ((type1 $tuser) (type2 $tuser))
  (unless (string= ($tuser.ident type1) ($tuser.ident type2))
    (error 
     (make-condition 'typecheck-internal-error
        :message "unmatched typed object found"
        :value (list type1 type2))))
  (mapcan 
    #'match
    ($tuser.args type1)
    ($tuser.args type2)))



(defmethod typecheck ((obj $special) env)
  (let ((ident ($special.ident obj)))
    (cond 
      ((string= ident "if")
       (multiple-value-bind (new-env args-type)
         (arg-typecheck-toplevel ($special.exprs obj) env)

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
                 (rule (simplify-rule (match thentype elsetype))))
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
    (arg-typecheck-toplevel ($call.exprs obj) env)
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
        ((or $fn $call)
          (multiple-value-bind (type new-env)
            (typecheck ident env)
            (lookup-function-type%
              ident type args-type new-env)))))))


(defun eliminate-ftype (ftype argtype env)
  "ftype を関数型 argtype を引数の型とするとき
   関数呼び出し後の型をもとめる
   必要に応じてenvを更新し、型と新しい環境を多値で返却する"
  (let ((domain ($tfunc.domain ftype))
        (range  ($tfunc.range ftype)))
    (cond 
      ((type= domain argtype) 
       (values range env))
      ((typep domain '$tundef)
       (values 
         (substype range domain argtype)
         env))
      ((typep argtype '$tundef)
       (values 
         range
         (update-env env argtype domain)))
      (t 
       (handler-case
           (let ((rule (simplify-rule (match domain argtype))))
             (values 
               (unify-type-with-rule range rule)
               (unify-env-with-rule env rule)))
           (typecheck-internal-error (c)
             (declare (ignore c))
             (error 
               (make-condition 'typecheck-error
                 :message "invalid function call"
                 :value (list domain argtype)))))))))
 


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
          (multiple-value-bind (rangetype new-env)
            (eliminate-ftype rtype at now-env)
            (setf now-env new-env)
            rangetype))
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
       (remove-if 
         (let (flag)
           (lambda (each)
           (destructuring-bind (nvar . ntype) each
             (setf flag (and (string= nvar var) (not flag))))))
         target-env)))
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


(defun make-function-schema (list rtype)
  "list is list of typedvar"
  (if (null list) 
    (if rtype rtype ($tundef))
    (let ((type ($typedvar.type (car list))))
      ($tfunc 
        (if type type ($tundef))
        (make-function-schema (cdr list) rtype)))))


(defmethod typecheck ((obj $def) env)
  (let* ((name ($def.name obj))
         (fn   ($def.fn obj))
         (rtype($fn.rtype fn))
         (function-schema (make-function-schema ($fn.arguments fn) rtype)))
    (multiple-value-bind (type new-env)
      (typecheck fn (acons name function-schema env))
      (values 
        type 
        (acons name type 
          (remove-if 
            (lambda (x) (string= x name))
            new-env
            :key #'car))))))



(defmethod typecheck ((obj $makenewtype) env)
  (let ((newtype      ($makenewtype.newtype obj))
        (constructors ($makenewtype.constructors obj)))
    (multiple-value-bind 
      (newnewtype rule)
      (rename newtype)
      "type List a = Cons a (List a) | Nilと
       type Maybe a = Just a | Nothing
       みたいな型変数が複数のところで出てくる定義があると
       match式がエラー吐く原因になるので一意な型変数になるように
       renameをする"
      (loop for each in constructors 
            do 
            (push 
              (cons (unify-type-with-rule each rule) newnewtype) *user-defined-type*))

      (values newnewtype env))))


(defun lookup-userdefined-type (userobj)
  "$userobjのコンストラクタを見てどの型($tuserオブジェクトを得る)"
  (assert (typep userobj '$userobj))
  (assoc ($userobj.ident userobj) 
         *user-defined-type*
         :key  #'$typecons.ident 
         :test #'string=))




(defun rename (obj)
  (assert (typep obj '$tuser))
  (let* ((args ($tuser.args obj))
         (rule
           (remove-duplicates
             (loop for each in args
                   if (typep each '$tundef)
                   collect (cons each ($tundef)))
             :test #'type=
             :key #'car)))  
    (values 
      (unify-type-with-rule obj rule)
      rule)))


(defmethod typecheck ((obj $userobj) env)
  "ユーザ定義した型のオブジェクトに対する型推論"
  (let ((tmp (lookup-userdefined-type obj)))
    
    (unless tmp
      (error "unknown constructor"))

    (destructuring-bind (constructor-schema . tuser) tmp
      (assert (and (typep constructor-schema '$typecons)
                   (typep tuser '$tuser)))

      ;; constructor-schema と userobjが一致するかを調べる
      ;; 一致すればtuserをに単一化を施しそれを返す

      (unless (= (length ($typecons.args constructor-schema))
                 (length ($userobj.args  obj)))
        (error "length of argument does not match"))

      (let* ((now env)
             (typemaching
               (simplify-rule
                 (loop 
                 for each in ($userobj.args obj)
                 for sc in ($typecons.args constructor-schema)
                 append
                 (multiple-value-bind (ty new) 
                   (typecheck each now)
                   (setf now new)
                   (match sc ty)))))) 

        (values 
          (unify-type-with-rule tuser typemaching)
          (unify-env-with-rule now typemaching)))

      )))









#|

(defmethod typecheck-clause  ((obj $match-clause) env)
  "match 式の一つの節が何型であるかを判定し
   その型と環境返す
   
   パターン中の$varオブジェクトを型環境に追加して
   (当然型変数)exprを評価する

   関数の引数と同じように型変数を導入するので unify-env-with-env しないと
   だめなはず。
   "
  (let ((pattern ($match-clause.pattern obj))
        (expr ($match-clause.expr obj)))
    (assert (typep pattern '$userobj))

    (multiple-value-bind (type new)
      (typecheck 
        expr
        (append
          (loop for arg in ($userobj.args pattern)
                if (typep arg '$var)
                collect (cons ($var.value arg) ($tundef)))
          env))
      (values 
        type 
        (unify-env-with-env env new)))))

(defun pattern-check (expr clauses env)
  "match式のパターン部のチェックする。
   値コンストラクタが全てtargetの式の型の値を生成するかのcheck
   通らないなら、エラー投げて、通れば新しい環境を返す"

  )

(defun expr-check (clauses env)
  "match式各節の右辺部部分の型推論を行う
   全ての節が同じ型を返すかのチェック。
   返すならばその型と環境を返す
   match l with 
     Cons<x,Nil<>> => Tuple<x,1>
     Cons<x,Cons<y,xs>> => Tuple<x,y>
     Cons<x,xs> => Tuple<5,x>
   のような式の各節を考えるとそれぞれ
   Tuple[type1 , Integer]
   Tuple[type2,type3]
   Tuple[Integer,type4]
   のようになる。
   これらは全て互いに単一化可能で無ければならない。
   この場合返すのは Tuple[Integer,Integer]であるが
   それは各フィールドでもっとも特定的な型を選択してゆけば良い"

  (let ((now env))
      (mapcar 
        (lambda (clause)
          (multiple-value-bind (type new)
            (typecheck-clause clause now)
            (setf now new)
            type))
        clauses)))

(defmethod typecheck ((obj $match) env)
  "match式は全ての節で同じ型を返さないといけない
   それの検査をする
   またこのメソッドを抜けるときにはパターン変数にマッチされた束縛を
   envから除去しなければならない"
  (let* ((expr ($match.expr obj))
         (clauses ($match.clauses obj)))
    (unless clauses 
      (error "null clauses is not allowed"))

    (expr-check clauses (pattern-check expr clauses env))))

|#


(defun collect-var (pattern)
  (etypecase pattern 
    ($var (list (cons ($var.value pattern) ($tundef))))
    ($userobj 
      (mapcan 
        #'collect-var
        ($userobj.args pattern)))
    (t (error "pattern required"))))


(defmethod typecheck ((obj $match) env)
  "match式のパターン部に来るのはコンストラクタの識別子と
   変数だけにする。
   Cons<1,Nil> とかのmatchはできない.
   なぜなら 様々なオブジェクトが 1　と等しいかを判断するためには
   等号がどのような型に対してどのように等値性を判定するかの定義が無いといけない
   Haskellとかだったら型クラスみたいな"
  (let* ((expr ($match.expr obj))
         (clauses ($match.clauses obj))) 

    (unless clauses 
      (error "null clauses is not allowed"))
  
    ;;; 先にmatch-clauseの式の部分を推論しなければ
    ;;; match に渡された式の型推論がうまくいかないはず
   (loop 
     named exit
     with most-specific-type = nil
     with renv = env
     finally (return-from exit (values most-specific-type renv))
     for clause in clauses
     for expr-type = (typecheck expr renv)
     for pattern = ($match-clause.pattern clause)
     for pattern-env = (collect-var pattern)
     for body    = ($match-clause.expr clause) 
     do 
     (multiple-value-bind 
       (body-type new-env)
       (typecheck body (append pattern-env renv))

       (when most-specific-type 
         (let ((rule (match body-type most-specific-type)))
           (setf body-type (unify-type-with-rule body-type rule)
                 new-env   (unify-env-with-rule new-env rule))))



       (let* ((pattern-type (typecheck pattern (unify-env-with-env pattern-env new-env)))
              (rule 
                (handler-case
                  (match expr-type pattern-type)
                  (t (c) 
                    (/ 1 0)
                    )
                  )))

         (setf renv 
               (unify-env-with-rule 
                 (set-difference new-env pattern-env :key #'car :test #'string=) 
                 rule)
               most-specific-type
               (unify-type-with-rule body-type rule))
         
         )))))



