

(in-package :cl-user)
(defpackage :protolang
  (:use :cl
        :protolang.errors
        :protolang.definition
        :protolang.reader
        :protolang.compiler))

(in-package :protolang)


#|
  コマンドライン引数の取得は処理系依存
|#


(defun argv ()
  #+sbcl sb-ext:*posix-argv*)

(defun quit ()
  #+sbcl (sb-ext:exit))

(defvar *default-name* "a.out")
(defvar *print-internal-expr* nil)
(defvar *print-sexpr* nil)
(defvar *print-env* nil)

(defun read-file (s)
  (loop 
    with result = ""
    finally (return result)
    for line = (read-line s nil nil)
    while line
    do (setf result (format nil "~A~%~A" result line))))
 
(defun output-executable (form dst)
  (in-package :plang-user)
  (eval form)
  (eval-when (:execute)
    (sb-ext:save-lisp-and-die 
      dst
      :toplevel #'plang-user::main
      :executable t
      :purify t)))

(defun debug-print (internal-objects sexpr env)
  "中間表現とS式とり、大域変数の値により
   それらを表示する"
  (when *print-internal-expr*
    (format *standard-output* 
            "~%DEBUG PRINT (Internal Expression)~%~%~t~A~%~%~%"
            internal-objects))
  (when *print-sexpr*
    (format *standard-output* 
            "~%DEBUG PRINT (S-Expression)~%~%~t~A~%~%~%"
            sexpr))
  (when (and *print-env* env)
    (format *standard-output* 
            "~%DEBUG PRINT (Type Environment)~%~%~{~t~A~%~}~%~%~%"
            env))
  (force-output *standard-output*))

(defun print-error-message (error-condition)
  (format *standard-output* "Error: ~A~%Message: ~A~%Value: ~A~%"
          (type-of error-condition)
          (message error-condition)
          (value error-condition)))

(defun handler (form)
  (handler-case 
    (funcall form)
    (plang-parse-error (c)
       (print-error-message c))
    (special-form (c)
       (print-error-message c))
    (typecheck-error (c)
       (print-error-message c))
    (typecheck-internal-error (c)
       (print-error-message c))
    (error (c)
       (declare (ignore c))
       (format *standard-output* "Error: ~A~%Message: unexpected error~%Value: ~A~%"
               (type-of c) c))))


(defun compile-source (name)
  (destructuring-bind (src . more) name
    (let ((dst (if (null more) *default-name* (car more))))
      (handler 
        (lambda ()
          (output-executable 
            (with-open-file (in src :direction :input)
              (->sexpr-toplevel-with-entry (plang-parser-toplevel (read-file in))))
            dst))))))


(defun prompt ()
  (format *standard-output* "~%>>> ")
  (force-output *standard-output*))

(defun printable-lisp-object (lisp-inner-object objects expr)
  (typecase lisp-inner-object
    (function objects)
    (boolean 
      (if lisp-inner-object "true" "false"))
    (t lisp-inner-object))
  )

(defun repl ()
  (prompt)
  (loop with env = nil
        for expr = (read-line *standard-input* nil nil)
        while expr
        when (string= expr "@+1") do (setf *print-sexpr* t)
        when (string= expr "@-1") do (setf *print-sexpr* nil)
        when (string= expr "@+2") do (setf *print-internal-expr* t)
        when (string= expr "@-2") do (setf *print-internal-expr* nil)
        when (string= expr "@+3") do (setf *print-env* t)
        when (string= expr "@-3") do (setf *print-env* nil)
        when (and (string/= expr "") (char/= #\@ (char expr 0))) do
        (handler
           (lambda ()
            (let ((objects (plang-parser-toplevel expr)))
                (multiple-value-bind (sexpr new-env type) 
                  (->sexpr-toplevel objects env)
                  (debug-print objects sexpr env)
                  (setf env new-env)
                  (format *standard-output* "~A : ~A~%"
                    (printable-lisp-object (eval sexpr) objects expr) type)
                  (force-output *standard-output*)))))
        do (prompt)))


(defun main ()
  ;;; sbcl のコマンドライン引数の1つめは 処理系名 であることを意図している
  (destructuring-bind (_ . more) (argv)
    (declare (ignore _))
    (if more
      (compile-source more)
      (repl))))

