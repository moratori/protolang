
(ql:quickload :protolang-test)
(use-package :protolang-test.typecheck)

(in-package :protolang-test.typecheck)



(let ((res (run-tests :all)))
  (print-errors res)
  (print-failures res)
  )


