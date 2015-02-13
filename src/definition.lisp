

(in-package :cl-user)
(defpackage :protolang.definition
  (:use :cl :cl-annot :cl-annot.class
        :protolang.errors
        ))
(in-package :protolang.definition)

(enable-annot-syntax)



@export-structure
(defstruct ($tundef (:constructor $tundef (&optional ident))
                    (:conc-name $tundef.)
                    (:print-object
                      (lambda (obj stream)
                        (format stream "~A" ($tundef.ident obj)))))
  (ident (symbol-name (gensym "tv"))))


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
(defstruct ($tfunc (:constructor $tfunc (domain range))
                   (:conc-name $tfunc.)
                   (:print-object 
                    (lambda (obj stream)
                      (format stream "(~A -> ~A)"
                              (print-object ($tfunc.domain obj) nil)
                              (print-object ($tfunc.range obj) nil)))))
  (domain nil :type (or $tint $tbool $tfunc $tundef))
  (range nil  :type (or $tint $tbool $tfunc $tundef)))









#|
  "type宣言で新しく作る型の左辺を表す
   identは新しい型名
   argsは型変数のリスト"  
|#
@export-structure 
(defstruct ($tuser (:constructor $tuser (ident args))
                   (:conc-name $tuser.)
                   (:print-object 
                     (lambda (obj stream)
                       (let ((args ($tuser.args obj)))
                         (if args
                           (format stream "(~A ~{~A~^ ~})" 
                                   ($tuser.ident obj)
                                   args)
                           (format stream "~A" ($tuser.ident obj)))))))
  (ident (error "ident required") :type string)
  (args nil :type list))


#|
  type宣言の右辺のバーで区切られる各フィールド
   identは構築子を識別するシンボル.
   構築子名の先頭は大文字にすることで関数とは区別する
   argsは型のリスト. 型変数も取りうる   
|#
@export-structure 
(defstruct ($typecons (:constructor $typecons (ident args))
                      (:conc-name $typecons.))
  (ident (error "ident required") :type string)
  (args nil :type list))


#|
|"type List a = Nil | Cons a (List a) みたいな表現の直接的な内部表現
   newtypeは $tuserのオブジェクト.(左辺)
   constructorsはnewtypeがどのような方法で作られ得るかを表す
   コンストラクタ($typeconsオブジェクト)のリスト
  この構造体は $tuser と $typecons から成り立つ
  S式にこれを落とすときはconstructors毎にdefstructで作ってやればおｋ   
| |#
@export-structure 
(defstruct ($makenewtype (:constructor $makenewtype (newtype constructors))
                         (:conc-name $makenewtype.)
                         )
  
  (newtype (error "newtype required") :type $tuser)
  (constructors nil :type list))

#|
"ユーザがtype宣言で定義した型を持つ値を保持するための構造体
  identは構築子名,argsはそれへの引数で、様々なオブジェクトが入るリスト
  Person <true,21>
  Point  <2,3>" 
|#
@export-structure 
(defstruct ($userobj (:constructor $userobj (ident args))
                     (:conc-name $userobj.)
                     (:print-object 
                       (lambda (obj stream)
                         (let ((args ($userobj.args obj)))
                           (if args 
                             (format stream "~A<~{~A~^,~}>"
                                 ($userobj.ident obj)
                                 ($userobj.args obj))
                             (format stream "~A" ($userobj.ident obj)))))))
  (ident (error "ident required") :type string)
  (args nil :type list))



@export-structure
(defstruct ($match-clause (:constructor $match-clause (pattern expr))
                          (:conc-name $match-clause.)
                          )
  (pattern (error "pattern required") :type $userobj)
  expr)


#|
| clausesは $match-clauseオブジェクトのリスト
|#
@export-structure
(defstruct ($match (:constructor $match (expr clauses))
                   (:conc-name $match.))
  expr 
  (clauses nil :type list))










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

@export
(defvar *user-defined-type* nil
  "コンストラクタとそれを表す型へのマッピング
   typeconsオブジェクトとtuserのacons")


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
 

