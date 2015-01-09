

(in-package :cl-user)
(defpackage :protolang.reader
  (:use :cl :cl-annot :cl-annot.class
        :protolang.errors
        :protolang.definition
        :yacc
        :cl-lex)
  (:export
    :plang-parser
    )
  )

(in-package :protolang.reader)



(defmacro %deflexer% (name &rest clauses)
  `(define-string-lexer ,name 
        ,@(mapcar (lambda (clause)
                    (destructuring-bind (pattern kind) clause
                      `(,pattern (return (values ,kind $@))))) clauses)))

(defmacro defparser (name defterm &rest body)
  (let ((lexer (gensym))
        (parser (gensym)))
    `(progn 
       (%deflexer% ,lexer ,@defterm)
       (define-parser ,parser 
          (:terminals
            ,(mapcar #'second defterm))            
          ,@body)
       (defun ,name (target)
         (parse-with-lexer (,lexer target) ,parser)))))




(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun make-bincall (left ident right)
    ($call ident (list left right)))

  (defun make-function-type (domain arrow range)
    (declare (ignore arrow))
    ($tfunc domain range))

  (defun make-monocall (ident expr)
    ($call ident (list expr)))

  (defun make-if (_ expr1 expr2 expr3)
    ($special _ (list expr1 expr2 expr3)))

  (defun make-function (arg ar expr)
    ($fn arg nil expr))

  (defun make-typed-function (arg colon rtype ar expr)
    (declare (ignore colon ar))
    ($fn arg rtype expr))

  (defun make-deffunction (def name fn)
    (declare (ignore def))
    ($def name fn))

  (defun make-general-function-call (callee real-arguments)
    ($call callee real-arguments))

  (defun ignore-paren (sparen expr eparen)
    (declare (ignore sparen eparen))
    expr) 
  
  )


(defparser plang-parser 
  (("[0-9]"      :integer) 
   ("true|false" :boolean) 
   ("Bool"       :tbool)   
   ("Int"        :tint)    
   ("def"        :def)
   ("if"         :if)
   ("->"         :arrow)   
   ("=="         :equal)   
   ("&&"         :and)     
   ("\\|\\|"     :or)      
   (":"          :colon)   
   ("!"          :not)     
   ("\\+"        :plus)    
   ("-"          :minus)   
   ("\\*"        :mult)    
   ("%"          :mod)     
   ("<"          :gt)      
   (","          :comma)   
   ("\\("        :sparen)  
   ("\\["        :sbracket) 
   ("\\)"        :eparen)  
   ("\\]"        :ebracket) 
   ("[a-zA-Z]+"   :variable))

  (:start-symbol program) 
  (:precedence
    ((:left :mult)
     (:left :mod)
     (:left :plus :minus :and :or)
     (:left :equal)
     (:left :if)
     (:left :arrow)
     (:right :not)
     (:left :print)))

  (program 
    (expr #'list)
    (expr program #'cons))

  (expr 
    literal
    var
    function
    (expr :plus expr  #'make-bincall)
    (expr :minus expr #'make-bincall)
    (expr :mod expr   #'make-bincall)
    (expr :mult expr  #'make-bincall)
    (expr :and expr   #'make-bincall)
    (expr :or expr    #'make-bincall)
    (expr :gt expr    #'make-bincall)
    (expr :equal expr #'make-bincall)
    (:not expr        #'make-monocall) 
    (expr real-arguments #'make-general-function-call)
    (:if expr expr expr #'make-if)
    (:def :variable function #'make-deffunction))
   
  (literal 
    (:integer #'$integer)
    (:boolean #'$boolean)
    (:sparen expr :eparen #'ignore-paren)) 
  
  (function
    (dummy-arguments :arrow expr #'make-function)
    (dummy-arguments :colon type :arrow expr #'make-typed-function))

  (dummy-arguments 
    (:sbracket varseq :ebracket #'ignore-paren))

  (real-arguments 
    (:sbracket exprseq :ebracket #'ignore-paren))

  (exprseq 
    (expr #'list)
    (exprseq :comma expr 
      (lambda (a b c)
        (declare (ignore b))
        (append a (list c)))))

  (varseq 
    (var 
      (lambda (a)
        (list ($typedvar a nil))))
    (var :colon type
      (lambda (a b c)
        (declare (ignore b))
        (list ($typedvar a c))))
    (varseq :comma var
      (lambda (a b c)
        (declare (ignore b))
        (append a (list ($typedvar c nil)))))
    (varseq :comma var :colon type
      (lambda (a b c d e)
        (declare (ignore b d))
        (append a ($typedvar c e)))))

  (var 
    (:variable #'$var))

  (type 
    (:tint  (lambda (x) (declare (ignore x)) ($tint)))
    (:tbool (lambda (x) (declare (ignore x)) ($tbool)))
    (type :arrow type #'make-function-type)
    (:sparen type :eparen #'ignore-paren)))


