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

;; sanitized-input list -> list
;; Construct a plist from an alist containing post or get parameters given from a request
(defun sanitize-input (parameters)
  (print parameters)
  (flet ((sanitize-string (parameter)
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
    ;;convert false to 0 and true to 1; C style which is accepted by mariadb... Look into this
    (loop for parameter in parameters 
       collect (intern (string-upcase (sanitize-string (car parameter))) :keyword)
       collect (let ((parameter-value (cdr parameter)))
                 (cond ((stringp parameter-value) (sanitize-string parameter-value))
                       ((symbolp parameter-value) (if (eq :false parameter-value)
                                                      0
                                                      1))
                       (t parameter-value))))))

(defun create-insert-statement (table-name user-input)
  (insert-into table-name
               (apply #'set= user-input)))

(defun add-new-record (table-name user-input search-column)
  (let* ((sanitized-user-input (sanitize-input user-input))
         (new-id 'nil))
    (if sanitized-user-input
        (progn (with-connection (db)
                 (execute (create-insert-statement table-name sanitized-user-input))
                 (setf new-id (getf (retrieve-one
                                     (select :id
                                             (from table-name)
                                             (where (:= search-column (getf sanitized-user-input search-column)))))
                                    :id)))
               (render-json `(:|status| "success" :|newid| ,new-id)))
        (render-json '(:|status| "error" :|code| "ENONAME")))))

;;Have an optional argument to sort-by
(defun retrieve-all-from-table (table-name)
  (with-connection (db)
    (retrieve-all
     (select :*
       (from table-name)))))

;; This is a small work around to what seems to be a bug in datafly. This should only be used for result sets!
(defun encode-json-custom (result-set)
  (if (> (length result-set) 1)
      (encode-json result-set)
      (concatenate 'string "[" (encode-json (car result-set)) "]")))

;;GET
(defroute ("/" :method :get) ()
  (let* ((initial-company-listing (encode-json-custom (retrieve-all-from-table :companies)))
         (initial-genre-listing (encode-json-custom (retrieve-all-from-table :genres)))
         (initial-systems-listing (encode-json-custom (retrieve-all-from-table :systems)))
         (environment-variables (list :companies initial-company-listing
                                      :genres initial-genre-listing
                                      :systems initial-systems-listing)))
    (render #p"index.html" environment-variables)))

;;POST
(defroute ("/company/" :method :post) (&key _parsed)
  (add-new-record :companies
                  _parsed
                  :name))

(defroute ("/genre/" :method :post) (&key _parsed)
  (add-new-record :genres
                  _parsed
                  :name))

(defroute ("/system/" :method :post) (&key _parsed)
  (add-new-record :systems
                  _parsed
                  :name))

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

(defroute ("/games/" :method :post) (&key |genres| |companies| _parsed)
  (let ((gameparameters (sanitize-input (filter-parameters _parsed '("genres" "companies"))))
        (new-game-id 'nil))
    (if (and |genres|
             |companies|
             _parsed)
        (with-connection (db)
          (execute
           (create-insert-statement :games gameparameters))
          (setf new-game-id (getf (retrieve-one
                              (select :id
                                      (from :games)
                                      (where (:= :name (getf gameparameters :name)))))
                                  :id))
          (loop for company-id in |companies| do
               (execute (create-insert-statement :games_companies_pivot `(:game_id ,new-game-id  :company_id ,company-id))))
          (loop for genre-id in |genres| do
               (execute (create-insert-statement :games_genres_pivot `(:game_id ,new-game-id :genre_id ,genre-id))))
          (render-json `(:|status| "success"  :|newid| ,new-game-id)))
        (render-json '(:|status| "error" :|code| "ENOTFILLED")))))

;;In progress
;;(defroute ("/search-games/" :method :post) (&key |genres| |companies| _parsed)
;;  (print
;;  (render-json `(:|foo| "bar")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
