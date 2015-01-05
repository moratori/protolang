
(defpackage protolang.asd
	(:use :cl :asdf))

(in-package protolang.asd)


(defsystem protolang
	:serial t
	:version "1.0"
	:author "moratori"
	:depends-on (:cl-lex :yacc :cl-annot)
	:components 
        ((:module "src"
          :serial t
          :components 
            ((:file "errors")
             (:file "definition")
             (:file "typecheck")
             (:file "reader")
             (:file "compiler")
             (:file "toplevel")))))
	


