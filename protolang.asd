
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
            ((:file "definition")
             (:file "typecheck")
             (:file "grammer")
             (:file "compiler")
             (:file "toplevel")))))
	


