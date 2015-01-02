
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(load "protolang.lisp")

(defun check (obj type)
  (type= (print (typecheck-toplevel obj)) type))


(define-test literal
   ;; 1 :: Integer
   (assert-true (check ($integer "1") ($tint)))
   ;; true :: Boolean
   (assert-true (check ($boolean "true") ($tbool))))

(define-test special-if
   ;; if true 1 3 :: Int
   (assert-true 
     (check ($special "if" (list ($boolean "true") ($integer "1") ($integer "3"))) ($tint)))
   ;; if (false && true) 1 3 :: Int
   (assert-true 
     (check ($special "if" (list ($call "&&" (list ($boolean "false") ($boolean "true"))) ($integer "1") ($integer "3"))) ($tint)))
   ;;if (false || true) 1 3
   (assert-true 
     (check ($special "if" (list ($call "||" (list ($boolean "false") ($boolean "true"))) ($integer "1") ($integer "3"))) ($tint)))
   ;; if !(false || true) 1 3 :: Int
   (assert-true 
     (check ($special "if" (list ($call "!" (list ($call "||" (list ($boolean "false") ($boolean "true"))))) ($integer "1") ($integer "3"))) ($tint)))
   ;; if !(!(1 == 2)) 1 2 :: Int
   (assert-true 
     (check ($special "if" (list ($call "!" (list ($call "!" (list ($call "==" (list ($integer "1") ($integer "2"))))))) ($integer "1") ($integer "3"))) ($tint))))

(define-test fn
  
   ;; (x:Int) => x :: Int -> Int
   (assert-true (check ($fn (list ($typedvar ($var "x") ($tint)))  nil ($var "x")) ($tfunc ($tint) ($tint))))
   ;; (x) => x + 1 :: Int -> Int
   (assert-true (check ($fn (list ($typedvar ($var "x") nil))  nil ($call "+" (list ($var "x") ($integer "1")))) ($tfunc ($tint) ($tint))))
   ;; (x):Int => x+1 :: Int -> Int
   (assert-true (check ($fn (list ($typedvar ($var "x") nil))  ($tint) ($call "+" (list ($var "x") ($integer "1")))) ($tfunc ($tint) ($tint))))
   ;; (f:(Int->Bool)) => if f[1] f (x) => x == 5 :: Int -> Bool
   (assert-true 
     (check ($fn (list ($typedvar ($var "f") ($tfunc ($tint) ($tbool)))) nil 
                 ($special "if" (list ($call "f" (list ($integer "1")))  ($var "f") ($fn (list ($typedvar ($var "x") nil)) nil ($call "==" (list ($var "x") ($integer "5")))) ) )) 
            ($tfunc ($tfunc ($tint) ($tbool)) ($tfunc ($tint) ($tbool)))))
   ;; (f) => if f[1] f (x) => x == 5 :: Int -> Bool
   (assert-true 
     (check ($fn (list ($typedvar ($var "f") nil )) nil 
                 ($special "if" (list ($call "f" (list ($integer "1")))  ($var "f") ($fn (list ($typedvar ($var "x") nil)) nil ($call "==" (list ($var "x") ($integer "5")))) ) )) 
            ($tfunc ($tfunc ($tint) ($tbool)) ($tfunc ($tint) ($tbool)))))
   
   )


(let ((result (run-tests '(literal special-if fn))))
  (print-errors result)
  (print-failures result)
  )




