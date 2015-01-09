

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
      (output-executable 
        (with-open-file (in src :direction :input)
          (handler
            (lambda () 
              (->sexpr-toplevel-with-entry (plang-parser-toplevel (read-file in))))))
        dst))))


(defun prompt ()
  (format *standard-output* "~%>>> ")
  (force-output *standard-output*))

(defun printable-lisp-object (lisp-object expr)
  (typecase lisp-object
    (function expr)
    (boolean 
      (if lisp-object "true" "false"))
    (t lisp-object)))

(defun repl ()
  (prompt)
  (loop with env = nil
        for expr = (read-line *standard-input* nil nil)
        while expr
        do
        (handler
          (lambda ()
            (when (string/= expr "")
              (let ((objects (plang-parser-toplevel expr)))
                (multiple-value-bind (sexpr new-env type) 
                  (->sexpr-toplevel objects env)
                  (setf env new-env)
                  (format *standard-output* "~A : ~A~%"
                    (printable-lisp-object (eval sexpr) expr) type)
                  (force-output *standard-output*))))))
        (prompt)))


(defun main ()
  ;;; sbcl のコマンドライン引数の1つめは 処理系名 であることを意図している
  (destructuring-bind (_ . more) (argv)
    (declare (ignore _))
    (if more
      (compile-source more)
      (repl))))

