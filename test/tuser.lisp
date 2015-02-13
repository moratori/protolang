

(in-package :cl-user)
(defpackage :protolang-test.tuser
  (:use :cl 
        :lisp-unit
        :protolang.definition
        :protolang.typecheck
        ))
(in-package :protolang-test.tuser)


(defun check (o)
  (print (typecheck o nil)))


(defun main () 
  (check 
    ($makenewtype 
        ($tuser "Gender" nil)
        (list 
          ($typecons "Male" nil)
          ($typecons "Female" nil))))
  (check 
    ($makenewtype 
        ($tuser "Person" nil)
        (list 
          ($typecons "Person"
                     (list ($tuser "Gender" nil)
                           ($tint)
                           ($tbool))))))
  (check 
    ($makenewtype 
      ($tuser "List" (list ($tundef "a")))
      (list 
        ($typecons "Nil" nil)
        ($typecons "Cons" 
          (list ($tundef "a") 
                ($tuser "List" (list ($tundef "a")))
                       )))))

  (check 
    ($userobj "Male" nil ))
  (check 
    ($userobj "Female" nil ))

  (check 
    ($userobj "Nil" nil))

  (check 
    ($userobj "Cons" 
              (list ($integer "1")
                    ($userobj "Nil" nil))))

  (check 
    ($userobj "Cons"
              (list ($boolean "true")
                    ($userobj "Cons"
              (list ($boolean "false")
                    ($userobj "Nil" nil))))))
  (check 
    ($userobj 
      "Person"
      (list 
        ($userobj "Female" nil )
        ($integer "27")
        ($boolean "true"))))

  
  (check 
    ($userobj "Cons"
      (list 
        ($userobj 
      "Person"
      (list 
        ($userobj "Female" nil )
        ($integer "27")
        ($boolean "true")))
        ($userobj "Cons"
                  (list 
                    ($userobj 
      "Person"
      (list 
        ($userobj "Male" nil )
        ($integer "21")
        ($boolean "false")))
                    ($userobj "Nil" nil)
                    )
                  ))))


  )










