(in-package :cl-user)

(defpackage :sanitizer-middleware
  (:use :cl :cl-ppcre)
  (:import-from :clack
                :<middleware>
                :call
                :call-next)
  (:import-from :http-body
                :parse)
  (:export <sanitizer-middleware>))

(in-package :sanitizer-middleware)

(defclass <sanitizer-middleware> (<middleware>) ())

(defmethod call ((this <sanitizer-middleware>) env)
  (append env `(:sanitized-parameters)
