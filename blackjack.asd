(asdf:defsystem #:blackjack
  :description "Blackjack card game, or 'veintiuno' ('veintiuna', as first mentioned in 'Rinconete y Cortadillo', from Miguel de Cervantes Saavedra)"
  :author "Sergio Navarro <sernamar@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "blackjack"))
  :in-order-to ((asdf:test-op (asdf:test-op :blackjack/tests))))

(asdf:defsystem #:blackjack/tests
  :description "Tests for blackjack"
  :author "Sergio Navarro <sernamar@pm.me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:blackjack
               #:fiveam)
  :pathname "t"
  :serial t
  :components ((:file "tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :fiveam :run!
                                                 (uiop:find-symbol* '#:all-tests '#:blackjack-tests))))
