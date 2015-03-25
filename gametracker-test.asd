(in-package :cl-user)
(defpackage gametracker-test-asd
  (:use :cl :asdf))
(in-package :gametracker-test-asd)

(defsystem gametracker-test
  :author "Bryan Baraoidan"
  :license ""
  :depends-on (:gametracker
               :prove)
  :components ((:module "t"
                :components
                ((:file "gametracker"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
