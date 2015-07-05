(in-package :cl-user)
(defpackage kuriboshoe-asd
  (:use :cl :asdf))
(in-package :kuriboshoe-asd)

(defsystem kuriboshoe
  :version "1.0"
  :author "Bryan Baraoidan"
  :license "LGPL"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql

               ;;Session storage
               :lack-session-store-dbi

               ;; Misc packages
               :cl-recaptcha
               :drakma)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "helpers"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "helpers")
                 (:file "config"))))
  :description "A simple tool to help keep track of my game collection"
  :in-order-to ((test-op (load-op kuriboshoe-test))))
