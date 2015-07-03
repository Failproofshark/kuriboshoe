(ql:quickload :kuriboshoe)

(defpackage kuriboshoe.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :kuriboshoe.web
                :*web*)
  (:import-from :kuriboshoe.config
                :config
                :productionp
                :*static-directory*))
(in-package :kuriboshoe.app)

(builder
 ;;We only want to serve from the application server on development.
 (unless (productionp)
   `(:static
     :path ,(lambda (path)
                    (if (ppcre:scan "^(?:/images/|/css/|/libs/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
                        path
                        nil))
     :root ,*static-directory*))
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     '(:backtrace
       :output (getf (config) :error-log))
     nil)
 :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
