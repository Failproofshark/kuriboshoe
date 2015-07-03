;; A collection of functions and macros I wrote to streamline a few things
(in-package :cl-user)
(defpackage kuriboshoe.helpers
  (:use :cl
        :caveman2
        :kuriboshoe.config
        :kuriboshoe.view
        :kuriboshoe.db
        :datafly
        :sxql)
  (:export :sanitize-string
           :sanitize-input
           :create-insert-statement
           :add-new-record
           :create-update-statement
           :update-record
           :retrieve-all-from-table
           :encode-json-custom
           :filter-parameters
           :populate-pivot-tables
           :create-delete-statement
           :parse-string-ints
           :guarantee-number
           :has-required-fields-p
           :add-to-table
           :update-table-entry
           :delete-from-table
           :session-protected-route
           :render-initial-page))
(in-package :kuriboshoe.helpers)

(defun sanitize-string (parameter)
  (concatenate 'string (loop for char across parameter
                          if (or (char= #\; char)
                                 (char= #\' char)
                                 (char= #\" char)
                                 (char= #\` char))
                          collect #\\ and collect char
                          else collect char)))

(defun sanitize-input (parameters)
  ;;Note: Is there a bug in sxql? When false is parsed by whatever is handing defroute the parameters
  ;;A symbol is returned for a boolean value which results in an error in execution. The sql statement
  ;;constructed by sxql looks correct so maybe its something with datafly's execute? Either way we
  ;;convert false to 0 and true to 1; C style which is accepted by mariadb... Look into this
  (loop for parameter in parameters 
     collect (intern (string-upcase (sanitize-string (car parameter))) :keyword)
     collect (let ((parameter-value (cdr parameter)))
               (cond ((stringp parameter-value) (sanitize-string parameter-value))
                     ((symbolp parameter-value) (if (eql :false parameter-value)
                                                    0
                                                    1))
                     (t parameter-value)))))

(defun create-insert-statement (table-name user-input)
  (insert-into table-name
               (apply #'set= user-input)))

(defun add-new-record (table-name user-input search-column)
  (let* ((sanitized-user-input (sanitize-input user-input))
         (new-id 'nil))
    (with-connection (db)
      (execute (create-insert-statement table-name sanitized-user-input))
      (setf new-id (getf (retrieve-one
                          (select :id
                                  (from table-name)
                                  (where (:= search-column (getf sanitized-user-input search-column)))))
                         :id)))
    (render-json `(:|status| "success" :|newid| ,new-id))))

(defun create-update-statement (table-name user-input id)
  (update table-name
          (apply #'set= user-input)
          (where (:= :id id))))

(defun update-record (table-name user-input id)
  (let ((sanitized-user-input (sanitize-input user-input)))
    (with-connection (db)
      (execute (create-update-statement table-name sanitized-user-input id)))))

(defmacro retrieve-all-from-table (table-name &body other-clauses)
  `(retrieve-all
    (select :*
            (from ,table-name)
            ,@other-clauses)))

;; This is a small work around to what seems to be a bug in datafly. This should only be used for result sets!
(defun encode-json-custom (result-set)
  (if (= (length result-set) 1)
      (concatenate 'string "[" (encode-json (car result-set)) "]")
      (encode-json result-set)))

(defun filter-parameters (parameters omitted-keys)
  (remove nil (map 'list
                   #'(lambda (parameter)
                       (let ((found-match (reduce #'(lambda (&optional boolean-1 boolean-2)
                                                      (or boolean-1 boolean-2))
                                                  (map 'list
                                                       #'(lambda (key) (string= (car parameter) key))
                                                       omitted-keys)
                                                  :initial-value 'nil)))
                         (unless found-match
                           parameter)))
                   parameters)))

(defun populate-pivot-tables (game-id genres companies)
  (loop for company-id in companies do
       (execute (create-insert-statement :games_companies_pivot `(:game_id ,game-id  :company_id ,company-id))))
  (loop for genre-id in genres do
       (execute (create-insert-statement :games_genres_pivot `(:game_id ,game-id :genre_id ,genre-id)))))

(defun create-delete-statement (table-name id)
  (delete-from table-name
               (where (:= :id id))))

;; We want to make sure we're inserting numbers not strings (just want to be sure we're inserting what we want)
(defun guarantee-number (var)
  (if (numberp var)
      var
      (parse-integer var)))
(defun parse-string-ints (list-of-strings)
  (map 'list
       #'guarantee-number
       list-of-strings))

(defun has-required-fields-p (required-arguments actual-arguments)
  (not (member 'nil (map 'list
                         #'(lambda (required-argument)
                             (cdr (assoc required-argument actual-arguments :test #'string=)))
                         required-arguments))))

(defun add-to-table (table-name required-fields user-input)
    (if (has-required-fields-p required-fields user-input)
      (add-new-record table-name
                      user-input
                      :name)
      (render-json `(:|status| "error" :|code| "EMALFORMEDINPUT"))))

(defun update-table-entry (id table-name required-fields user-input)
  (if (and id
           (has-required-fields-p required-fields user-input))
      (handler-case (let ((parsed-id (guarantee-number id)))
                      (update-record table-name user-input parsed-id)
                      (render-json '(:|status| "success")))
        (sb-int:simple-parse-error ()
          (render-json '(:|status| "error" :|code| "ENOTINT"))))
      (render-json '(:|status| "error" :|code| "EMALFORMEDINPUT"))))

(defun delete-from-table (id table)
    (if id
      (handler-case (let ((parsed-id (guarantee-number id)))
                      (with-connection (db)
                        (execute
                         (create-delete-statement table parsed-id)))
                      (render-json '(:|status| "success")))
        (sb-int:simple-parse-error ()
          (render-json '(:|status| "error" :|code| "ENOTINT"))))
      (render-json '(:|status| "error" :|code| "EMALFORMEDINPUT"))))

(defmacro session-protected-route ((response-type) &body body)
  `(if (gethash :isverified *session* nil)
       (progn ,@body)
       (progn (setf (status *response*) 403)
              ,(if (eql response-type :json)
                   `(encode-json-custom '(:|status| "error" :|code| "EUNAUTH"))
                   `(throw-code 403)))))

(defun render-initial-page (template)
  (let* ((initial-company-listing)
         (initial-genre-listing)
         (initial-systems-listing))
    (with-connection (db)
      (setf initial-company-listing (encode-json-custom (retrieve-all-from-table :companies)))
      (setf initial-genre-listing (encode-json-custom (retrieve-all-from-table :genres)))
      (setf initial-systems-listing (encode-json-custom (retrieve-all-from-table :systems))))
    (render template (list :companies initial-company-listing
                           :genres initial-genre-listing
                           :systems initial-systems-listing))))
