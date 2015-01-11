

(in-package :cl-user)
(defpackage :protolang-test.typecheck
  (:use :cl 
        :lisp-unit
        :protolang.definition
        :protolang.typecheck
        ))
(in-package :protolang-test.typecheck)


(defun check (obj type)
  (type= (typecheck obj nil) type))


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


(define-test call 
   
   ;; ((x) => x + 1) 100
   (assert-true
     (check 
       ($call 
         ($fn (list ($typedvar ($var "x") nil)) nil 
              ($call "+" (list ($var "x") ($integer "1"))))
         (list ($integer "100")))
       ($tint)))
   ;; ((x) => (y) => x * y) 5
   (assert-true
     (check 
       ($call 
         ($fn (list ($typedvar ($var "x") nil)) nil 
              ($fn (list ($typedvar ($var "y") nil)) nil
                   ($call "*" (list ($var "x") ($var "y")))))
         (list ($integer "100")))
       ($tfunc ($tint) ($tint))))
   ;; ((x) => x) 100
   (assert-true
     (check 
       ($call 
         ($fn (list ($typedvar ($var "x") nil)) nil  ($var "x"))
         (list ($integer "100")))
       ($tint)))
   ;; ((x) => (y) => x+y) [1][2]
   (assert-true
     (check 
       ($call 
         ($call 
       ($fn (list ($typedvar ($var "x") nil)) nil 
                 ($fn (list ($typedvar ($var "y") nil)) nil 
                      ($call "+" (list ($var "x") ($var "y")))))
       (list ($integer "1")))
         (list ($integer "2"))
         )
       ($tint)
       )
     )
   ;; ((f) => f(1)) (x) => x*5
   (assert-true
     (check 
       ($call
         ($fn (list ($typedvar ($var "f") nil)) nil ($call "f" (list ($integer "1"))))
         (list ($fn (list ($typedvar ($var "x") nil)) nil ($call "*" (list ($var "x") ($integer "5")))))
         )
       ($tint)
       )
     )
 )

(define-test fn

   ;; (x) => if x 1 2
   (assert-true 
     (check ($fn (list ($typedvar ($var "x") nil))  nil 
                 ($special "if" (list ($var "x") ($integer "1") ($integer "2")))) 
            ($tfunc ($tbool) ($tint)))) 
   ;; (x:Int) => x :: Int -> Int
   (assert-true 
     (check ($fn (list ($typedvar ($var "x") ($tint))) nil 
                 ($var "x")) 
            ($tfunc ($tint) ($tint))))
   ;; (x) => (y) => x+y
   (assert-true 
     (check ($fn (list ($typedvar ($var "x") nil)) nil 
                 ($fn (list ($typedvar ($var "y") nil)) nil 
                      ($call "+" (list ($var "x") ($var "y"))))) 
            ($tfunc ($tint) ($tfunc ($tint) ($tint)))))
   ;; (x) => x + 1 :: Int -> Int
   (assert-true 
     (check ($fn (list ($typedvar ($var "x") nil)) nil 
                 ($call "+" (list ($var "x") ($integer "1")))) 
            ($tfunc ($tint) ($tint))))
   ;; (x) => (+) x :: Int -> (Int -> Int)
   (assert-true 
     (check ($fn (list ($typedvar ($var "x") nil)) nil 
                 ($call "+" (list ($var "x") ))) 
            ($tfunc ($tint) ($tfunc ($tint) ($tint)))))
   ;; (x):Int => x+1 :: Int -> Int
   (assert-true 
     (check ($fn (list ($typedvar ($var "x") nil)) ($tint) 
                 ($call "+" (list ($var "x") ($integer "1")))) 
            ($tfunc ($tint) ($tint)))) 
   ;; (x) => (y) => x + y :: Int -> (Int -> Int)
   (assert-true 
     (check ($fn (list ($typedvar ($var "x") nil )) nil 
                 ($fn (list ($typedvar ($var "y") nil)) nil
                      ($call "+" (list ($var "x") ($var "y")))))
            ($tfunc ($tint) ($tfunc ($tint) ($tint)))))
   ;; (x:Int) => (x:Bool) => !x  :: (Int -> (Bool -> Bool))
   (assert-true
     (check ($fn (list ($typedvar ($var "x") ($tint))) nil
                 ($fn (list ($typedvar ($var "x") ($tbool))) nil
                      ($call "!" (list ($var "x")))))
            ($tfunc ($tint) ($tfunc ($tbool) ($tbool)))))
   ;; (x:Int) => (x) => ! x :: (Int -> (Bool -> Bool))
   (assert-true
     (check ($fn (list ($typedvar ($var "x") ($tint))) nil
                 ($fn (list ($typedvar ($var "x") ($tbool))) nil
                      ($call "!" (list ($var "x")))))
            ($tfunc ($tint) ($tfunc ($tbool) ($tbool)))))
   ;; (f:(Int->Bool)) => if f[1] f (x) => x == 5 :: (Int -> Bool) -> (Int -> Bool)
   (assert-true 
     (check ($fn (list ($typedvar ($var "f") ($tfunc ($tint) ($tbool)))) nil 
                 ($special "if" (list ($call "f" (list ($integer "1")))  ($var "f") ($fn (list ($typedvar ($var "x") nil)) nil ($call "==" (list ($var "x") ($integer "5")))) ) )) 
            ($tfunc ($tfunc ($tint) ($tbool)) ($tfunc ($tint) ($tbool)))))
   ;; (f) => if true f[1] 2 :: (Int -> Int) -> Int
   (assert-true 
     (check ($fn (list ($typedvar ($var "f") nil))  nil 
                 ($special "if" (list ($boolean "true") ($call "f" (list ($integer "1"))) ($integer "2")))) 
            ($tfunc ($tfunc ($tint) ($tint)) ($tint))))
   ;; (f) => if f[1] f (x) => x == 5 :: (Int -> Bool) -> (Int -> Bool)
   (assert-true 
     (check ($fn (list ($typedvar ($var "f") nil )) nil 
                 ($special "if" (list ($call "f" (list ($integer "1")))  
                                      ($var "f") 
                                      ($fn (list ($typedvar ($var "x") nil)) nil ($call "==" (list ($var "x") ($integer "5"))))))) 
            ($tfunc ($tfunc ($tint) ($tbool)) ($tfunc ($tint) ($tbool)))))
   )


(define-test def
   ;; def add (x) -> x+1
   (assert-true 
     (check 
       ($def "add" ($fn (list ($typedvar ($var "x") nil)) nil ($call "+" (list ($var "x") ($integer "1")))))
       ($tfunc ($tint) ($tint))))
   ;; def fact (x:Int):Int -> if (x == 0) 1 (n * fact(n-1))
   (assert-true 
     (check 
       ($def "fact" ($fn (list ($typedvar ($var "x") ($tint))) ($tint) 
                         ($special "if" (list ($call "==" (list ($var "x") ($integer "0"))) 
                                              ($integer "1") 
                                              ($call "*" (list ($var "x") ($call "fact" (list ($call "-" (list ($var "x") ($integer "1")))))))))))
       ($tfunc ($tint) ($tint))))
   ;; def fact (x):Int -> if (x == 0) 1 (n * fact(n-1))
   (assert-true 
     (check 
       ($def "fact" ($fn (list ($typedvar ($var "x") nil)) ($tint) 
                         ($special "if" (list ($call "==" (list ($var "x") ($integer "0"))) 
                                              ($integer "1") 
                                              ($call "*" (list ($var "x") ($call "fact" (list ($call "-" (list ($var "x") ($integer "1")))))))))))
       ($tfunc ($tint) ($tint))))
   ;; def fact (x) -> if (x == 0) 1 (n * fact(n-1))
   (assert-true
     (check 
       ($def "fact" ($fn (list ($typedvar ($var "x") nil))  nil
                         ($special "if" (list ($call "==" (list ($var "x") ($integer "0"))) 
                                              ($integer "1") 
                                              ($call "*" (list ($var "x") ($call "fact" (list ($call "-" (list ($var "x") ($integer "1")))))))))))
       ($tfunc ($tint) ($tint))))
   ;; def search[f,init]: Int -> if f[init] init search[f,init+1]
   (assert-true
     (check 
       ($def "search" ($fn (list ($typedvar ($var "f") nil) ($typedvar ($var "init") nil)) ($tint)
                         ($special "if" (list ($call "f" (list ($var "init"))) 
                                              ($var "init") 
                                              ($call "search" (list ($var "f") ($call "+" (list ($var "init") ($integer "1")))))))))
       ($tfunc ($tfunc ($tint) ($tbool)) ($tfunc ($tint) ($tint)))))
   ;; def search[f,init:Int]: Int -> if f[init] init search[f,init+1]
   (assert-true
     (check 
       ($def "search" ($fn (list ($typedvar ($var "f") nil) ($typedvar ($var "init") ($tint))) ($tint)
                         ($special "if" (list ($call "f" (list ($var "init"))) 
                                              ($var "init") 
                                              ($call "search" (list ($var "f") ($call "+" (list ($var "init") ($integer "1")))))))))
       ($tfunc ($tfunc ($tint) ($tbool)) ($tfunc ($tint) ($tint)))))
   ;; def search[f,init] -> if f[init] init search[f,init+1]
   (assert-true
     (check 
       ($def "search" ($fn (list ($typedvar ($var "f") nil) ($typedvar ($var "init") nil)) nil
                         ($special "if" (list ($call "f" (list ($var "init"))) 
                                              ($var "init") 
                                              ($call "search" (list ($var "f") ($call "+" (list ($var "init") ($integer "1")))))))))
       ($tfunc ($tfunc ($tint) ($tbool)) ($tfunc ($tint) ($tint)))))

   )



(defun run (tests) 
  (let ((result (run-tests tests)))
    (print-errors result)
    (print-failures result)))




