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
(defun sanitize-string (parameter)
  (concatenate 'string (loop for char across parameter
                          if (or (char= #\; char)
                                 (char= #\' char)
                                 (char= #\" char)
                                 (char= #\` char))
                          collect #\\ and collect char
                          else collect char)))

;; sanitized-input list -> list
;; Construct a plist from an alist containing post or get parameters given from a request
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

(defun create-update-statement (table-name user-input id)
  (update table-name
          (apply #'set= user-input)
          (where (:= :id id))))

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

;;Have an optional argument to sort-by
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

(defun delete-from-table (table-name id)
  (delete-from table-name
               (where (:= :game_id id))))

;;GET
(defroute ("/" :method :get) ()
  ;; We don't assign the following variables initial values so we can just open the db once to request all the data we need
  (let* ((initial-company-listing)
         (initial-genre-listing)
         (initial-systems-listing))
    (with-connection (db)
      (setf initial-company-listing (encode-json-custom (retrieve-all-from-table :companies)))
      (setf initial-genre-listing (encode-json-custom (retrieve-all-from-table :genres)))
      (setf initial-systems-listing (encode-json-custom (retrieve-all-from-table :systems))))
    (render #p"index.html" (list :companies initial-company-listing
                                 :genres initial-genre-listing
                                 :systems initial-systems-listing))))

(defroute ("/games/" :method :get) (&key |id|)
  (if |id|
      (handler-case (flet ((extract-if-single (suspect-list)
                             ;;This was written in light of the datafly bug I found concerning single item lists
                             (if (= (length suspect-list) 1)
                                 (car suspect-list)
                                 suspect-list)))
                      (let ((game-id (parse-integer |id|))
                            (game-table-record)
                            (related-genres)
                            (related-companies)
                            (systems)
                            (genres)
                            (companies))
                        (with-connection (db)
                          (setf game-table-record (retrieve-one
                                                   (select :*
                                                           (from :games)
                                                           (where (:= :id game-id)))))
                          (setf related-genres (list :genres (extract-if-single (retrieve-all-from-table :games_genres_pivot
                                                                                                         (where (:= :game_id game-id))))))
                          (setf related-companies (list :companies (extract-if-single (retrieve-all-from-table :games_companies_pivot
                                                                                                               (where (:= :game_id game-id))))))
                          (setf systems (list :new-systems (extract-if-single (retrieve-all-from-table :systems))))
                          (setf genres (list :new-genres (extract-if-single (retrieve-all-from-table :genres))))
                          (setf companies (list :new-companies (extract-if-single (retrieve-all-from-table :companies))))
                          (setf (headers *response* :contente-type) "application/json")
                          (encode-json-custom (append game-table-record
                                                      related-genres
                                                      related-companies
                                                      systems
                                                      genres
                                                      companies)))))
        (sb-int:simple-parse-error ()
          (render-json '(:|status| "error" :|code| "ENOTINT"))))
      (render-json `(:|status| "error" :|code| "ENOID"))))
                                
(defun has-required-fields-p (required-arguments actual-arguments)
  (not (member 'nil (map 'list
                         #'(lambda (required-argument)
                             (cdr (assoc required-argument actual-arguments :test #'string=)))
                         required-arguments))))
       

;;POST
(defroute ("/company/" :method :post) (&key _parsed)
  (if (has-required-fields-p '("name" "is_manufacturer") _parsed)
      (add-new-record :companies
                      _parsed
                      :name)
      (render-json `(:|status| "error" :|code| "EMALFORMEDINPUT"))))

(defroute ("/genre/" :method :post) (&key _parsed)
  (if (has-required-fields-p '("name") _parsed)
      (add-new-record :genres
                      _parsed
                      :name)
      (render-json `(:|status| "error" :|code| "EMALFORMEDINPUT"))))

(defroute ("/system/" :method :post) (&key _parsed)
  (if (has-required-fields-p '("name" "manufacturer_id") _parsed)
      (add-new-record :systems
                      _parsed
                      :name)
      (render-json `(:|status| "error" :|code| "EMALFORMEDINPUT"))))

(defroute ("/games/" :method :post) (&key |genres| |companies| _parsed)
  (if (and |genres|
           |companies|
           (has-required-fields-p '("name" "region" "has_manual" "has_box" "quantity" "system_id") _parsed))
      (let ((gameparameters (sanitize-input (filter-parameters _parsed '("genres" "companies"))))
            (new-game-id 'nil))
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
          (render-json `(:|status| "success"  :|newid| ,new-game-id))))
      (render-json '(:|status| "error" :|code| "EMALFORMEDINPUT"))))


(defroute ("/search-games-ajax/" :method :post) (&key |genres| |companies| _parsed)
  ;;We sanitize the values which are then placed in an and list
  (if (or |genres|
          |companies|
          _parsed)
      (flet ((create-or-list (id-column-name id-list)
               (list (append '(:or) (map 'list
                                         #'(lambda (id)
                                             `(:= ,id-column-name ,id))
                                         id-list)))))
        (let* ((game-parameters (remove 'nil
                                        (map 'list
                                             #'(lambda (parameter)
                                                 (let ((keyword-symbol (intern (string-upcase (car parameter)) :keyword)))
                                                   (if (and (not (eql keyword-symbol :companies))
                                                            (not (eql keyword-symbol :genres)))
                                                       (if (eql :name keyword-symbol)
                                                           `(:like :games.name ,(concatenate 'string
                                                                                             "%"
                                                                                             (sanitize-string (cdr parameter))
                                                                                             "%"))
                                                           `(:= ,keyword-symbol ,(sanitize-string (cdr parameter)))))))
                                             _parsed)))
               (company-parameters (if |companies| (create-or-list :company_id |companies|)))
               (genre-parameters (if |genres| (create-or-list :genre_id |genres|)))
               (where-arguments (append '(:and)
                                        game-parameters
                                        company-parameters
                                        genre-parameters))
               (result-set 'nil))
          (with-connection (db)
            (setf result-set
                  (retrieve-all
                   (select (:games.*
                            (:as :systems.name :system_name)
                            (:as :genres.id :genre_id)
                            (:as :genres.name :genre_name)
                            (:as :companies.id :company_id)
                            (:as :companies.name :companies_name))
                           (from :games)
                           (inner-join :systems :on (:= :games.system_id :systems.id))
                           (inner-join :games_genres_pivot :on (:= :games.id :games_genres_pivot.game_id ))
                           (inner-join :genres :on (:= :games_genres_pivot.genre_id :genres.id))
                           (inner-join :games_companies_pivot :on (:= :games_companies_pivot.game_id :games.id))
                           (inner-join :companies :on (:= :games_companies_pivot.company_id :companies.id))
                           (where where-arguments)
                           (group-by :games.id)))))
          (setf (headers *response* :content-type) "application/json")
          (encode-json-custom result-set)))
      (render-json `(:|status| "error" :|code| "EMALFORMEDINPUT"))))

;; PUT
(defroute ("/games/" :method :put) (&key |id| |genres| |companies| _parsed)
  (if (and |id|
           |genres|
           |companies|
           (has-required-fields-p '("name" "region" "has_manual" "has_box" "quantity" "system_id") _parsed))
      (handler-case (let ((game-parameters (sanitize-input (filter-parameters _parsed '("genres" "companies"))))
                          (parsed-game-id (parse-integer |id|))
                          (parsed-company-ids (map 'list #'parse-integer |companies|))
                          (parsed-genre-ids (map 'list #'parse-integer |genres|)))
                      (with-connection (db)
                        (execute
                         (create-update-statement :games game-parameters parsed-game-id))
                        (execute
                         (delete-from-table :games_genres_pivot parsed-company-ids))
                        (execute
                         (delete-from-table :games_companies_pivot parsed-game-id))
                        (populate-pivot-tables parsed-game-id parsed-company-ids parsed-genre-ids))
                      (render-json '(:|status| "success")))
        (sb-int:simple-parse-error ()
          (render-json '(:|status| "error" :|code| "ENOTINT"))))
      (render-json '(:|status| "error" :|code| "EMALFORMEDINPUT"))))

;; DELETE

;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
