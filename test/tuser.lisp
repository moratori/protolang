

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


(defun test1 ()

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
                ($tuser "List" (list ($tundef "a"))))))))

  (check 
    ($makenewtype 
      ($tuser "Maybe" (list ($tundef "a")))
      (list 
        ($typecons "Nothing" nil)
        ($typecons "Just" (list ($tundef "a")))
        )
      )
    )

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
                    ($userobj "Nil" nil))))))

  (check 
    ($userobj "Nothing" nil))
  
  (check 
    ($userobj "Just" (list ($userobj "Female" nil))))

  
    (check ($def "cons" 
          ($fn (list ($typedvar ($var "x") nil)
                     ($typedvar ($var "xs") nil)) nil 
               ($userobj "Cons" 
                         (list ($var "x") ($var "xs"))))))
    
    (check ($def "func" 
          ($fn (list ($typedvar ($var "x") nil)) nil 
               ($userobj "Just" 
                         (list ($var "x"))))))
    
    (check 
      ($def "f" 
            ($fn (list ($typedvar ($var "x") nil)
                       ($typedvar ($var "y") nil)) nil
                 ($userobj "Cons"
                           (list ($userobj "Just" (list ($var "x")))
                                 ($var "y"))))))
    (check 
      ($def "h" 
            ($fn (list ($typedvar ($var "x")  nil)
		                  ($typedvar ($var "y")  nil)
                      ($typedvar ($var "xs") nil)) nil
      ($userobj "Cons" (list ($var "x") ($userobj "Cons" (list ($var "y") ($var "xs"))))))))


)

(defun test2 ()
  ;; (List Integer) -> Integer
  (check 
    ($def "f" ($fn (list ($typedvar ($var "l") nil)) nil
                   ($match ($var "l")
                           (list 
                             ($match-clause 
                               ($userobj "Nil" nil) 
                               ($integer "0"))
                             ($match-clause 
                               ($userobj "Cons" (list ($var "x") ($userobj "Nil" nil)))
                               ($integer "1"))
                             ($match-clause 
                               ($userobj "Cons" (list ($var "x") ($var "xs")))
                               ($var "x"))))))
    )

  )



(defun test3 ()
  (check ($makenewtype 
      ($tuser "Tree" (list ($tundef "a")))
      (list 
        ($typecons "Leaf" (list ($tundef "a")))
        ($typecons "Node" 
          (list ($tundef "a") 
                ($tuser "Tree" (list ($tundef "a")))
                ($tuser "Tree" (list ($tundef "a")))
                 )))))
  (check 
    ($userobj 
      "Leaf"
      (list ($integer "1"))))

  (check 
    ($userobj 
      "Node"
      (list 
        ($boolean "true")
        ($userobj "Leaf" (list ($boolean "false")))
        ($userobj 
      "Node"
      (list 
        ($boolean "true")
        ($userobj "Leaf" (list ($boolean "false")))
        ($userobj "Leaf" (list ($boolean "true"))))))))
  
  )







