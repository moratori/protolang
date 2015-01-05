

(in-package :cl-user)
(defpackage :protolang-test.compiler
  (:use :cl 
        :lisp-unit
        :protolang.definition
        :protolang.compiler
        ))
(in-package :protolang-test.compiler)


(defmacro check (fun obj assert &optional (flag t))
  (if flag
    `(assert-true (funcall ,fun (eval (->sexpr ,obj)) ,assert)) 
    `(assert-true (funcall ,fun (->sexpr ,obj) ,assert))))


(define-test literal
  (check #'= ($integer "1") 1)
  (check #'= ($integer "1") 1)
  (check #'eql ($boolean "true") t)
  (check #'eql ($boolean "false") nil)
  (check #'eq ($var "FOO") 'plang-user::FOO nil)
  )


(defun show ()
  (dolist (each 
            (list 
        (list ($call "+" (list ($integer "1") ($integer "2"))))
        (list ($special "if" (list ($boolean "true") ($integer "1") ($integer "2"))))
        (list ($fn (list ($typedvar ($var "f") nil )) nil 
                 ($special "if" (list ($call "f" (list ($integer "1")))  
                                      ($var "f") 
                                      ($fn (list ($typedvar ($var "x") nil)) nil ($call "==" (list ($var "x") ($integer "5"))))))))
        (list ($def "fact" ($fn (list ($typedvar ($var "x") nil)) ($tint) 
                         ($special "if" (list ($call "==" (list ($var "x") ($integer "0"))) 
                                              ($integer "1") 
                                              ($call "*" (list ($var "x") ($call "fact" (list ($call "-" (list ($var "x") ($integer "1")))))))))))
              ($call "fact" (list ($integer "5")))
              )
        
        )
            )
    
    (print (->sexpr-toplevel each))
    )
  )

(defun run (tests) 
  (let ((result (run-tests tests)))
    (print-errors result)
    (print-failures result)))





