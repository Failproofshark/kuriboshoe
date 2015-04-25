(in-package :cl-user)
(defpackage sanitizer-middleware-asd
  (:use :cl :asdf))

(defsystem sanitizer-middleware
  :version "0.1"
  :author "Bryan Baraoidan"
  :license "LGPL"
  :depends-on (:clack
               :http-body
               :cl-ppcre)
  :components ((:file "sanitizer-middleware"))
  :description "A catch all middleware that escapes input before passing the rest of the environment along.")
