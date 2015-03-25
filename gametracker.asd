(in-package :cl-user)
(defpackage gametracker-asd
  (:use :cl :asdf))
(in-package :gametracker-asd)

(defsystem gametracker
  :version "0.1"
  :author "Bryan Baraoidan"
  :license "LGPL"
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "A simple tool to help keep track of my game collection."
  :in-order-to ((test-op (load-op gametracker-test))))
