(in-package :cl-user)
(defpackage gametracker.web
  (:use :cl
        :caveman2
        :gametracker.config
        :gametracker.view
        :gametracker.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :gametracker.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules
(defun sanitize-input (parameters)
  (flet ((sanitize-parameter (parameter)
           (concatenate 'string (loop for char across parameter
                                   if (or (char= #\; char)
                                          (char= #\' char)
                                          (char= #\" char)
                                          (char= #\` char))
                                   collect #\\ and collect char
                                   else collect char))))
    ;;Note: Is there a bug in sxql? When false is parsed by whatever is handing defroute the parameters
    ;;A symbol is returned for a boolean value which results in an error in execution. The sql statement
    ;;constructed by sxql looks correct so maybe its something with datafly's execute? Either way we
    ;;convert false to 0 and true to 1;C style which is accepted by mariadb... Look into this
    (loop for parameter in parameters 
       collect (intern (string-upcase (car parameter)) :keyword)
       collect (let ((parameter-value (cdr parameter)))
                 (cond ((stringp parameter-value) (sanitize-parameter parameter-value))
                       ((symbolp parameter-value) (if (eq :false parameter-value)
                                                      0
                                                      1))
                       (t parameter-value))))))

(defun add-new-record (table-name user-input search-column)
  (let* ((sanitized-user-input (sanitize-input user-input))
         (new-id 'nil))
    (if sanitized-user-input
        (progn (with-connection (db)
                 (execute
                  (insert-into table-name
                               (apply #'set= sanitized-user-input)))
                 (render-json `(:|status| "success" :|newid| ,new-id))
                 (setf new-id (getf (retrieve-one
                                     (select :id
                                             (from table-name)
                                             (where (:= search-column (getf sanitized-user-input search-column)))))
                                    :id)))
               (render-json `(:|stats| "success" :|newid| ,new-id)))
        (render-json '(:|status| "error" :|code| "ENONAME")))))

(defroute ("/" :method :get) ()
  (render #p"index.html"))

(defroute ("/company/" :method :post) (&key _parsed)
  (add-new-record :companies
                  _parsed
                  :name))
                  
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
