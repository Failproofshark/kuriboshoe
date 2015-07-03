(in-package :cl-user)
(defpackage kuriboshoe-test-asd
  (:use :cl :asdf))
(in-package :kuriboshoe-test-asd)

(defsystem kuriboshoe-test
  :author ""
  :license ""
  :depends-on (:kuriboshoe
               :prove)
  :components ((:module "t"
                :components
                ((:file "kuriboshoe"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
