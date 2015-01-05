


(defpackage protolang-test.asd
	(:use :cl :asdf))

(in-package protolang-test.asd)


(defsystem protolang-test
	:serial t
	:version "1.0"
	:author "moratori"
	:depends-on (:lisp-unit :protolang)
	:components 
        ((:module "test"
          :serial t
          :components 
            ((:file "typecheck")
             (:file "compiler")))))
