

(in-package :cl-user)
(defpackage :protolang-test.reader
  (:use :cl 
        :lisp-unit
        :protolang.definition
        :protolang.reader
        ))
(in-package :protolang-test.reader)




(defun show ()
  (dolist (each
            '(
              "1+2"
              "1+2+3"
              "1*2+3%4"
              "if true 1 2"
              "[x]-> x"
              "f[1,2]"
              "1 2 3"
              "1 2"
              "([x] -> x)[1]"
              "x * fact[x-1]"
              "def fact[x:Int]:Int ->if (x == 0) 1 x * fact[x-1]"
              "def fact[x]:Int->if (x == 0) 1 x * fact[x-1]"
              "def fib[x:Int] -> if (x < 2) x fib[x-2]+fib[x-1]"
              "def fib[x] -> if (x < 2) x fib[x-2]+fib[x-1]"
              "[x]->[y]->x+y"
              "true && false || true"
              "f[1,2,3]"
              "([x]->[y]->[z]->x*y+z)[1,2,3]"
              ))
    (print (plang-parser each))))

