(in-package :cl-user)
(defpackage gametracker.web
  (:use :cl
        :caveman2
        :gametracker.config
        :gametracker.view
        :gametracker.db
        :gametracker.helpers
        :datafly
        :drakma
        :cl-json
        :flexi-streams
        :sxql)
  (:shadowing-import-from :datafly :encode-json)
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
;;GET
(defroute ("/" :method :get) ()
  (render-initial-page #p"index.html"))

(defroute ("/admin" :method :get) (&key |error|)
  (let ((error-message (cond ((string= |error| "EINCORR") "Incorrect name and password combination")
                             ((string= |error| "ELOCKED") "Too many attempts have been made. You have been locked out of the system")
                             ((string= |error| "EMISSING") "Please enter a username and password")
                             ((string= |error| "missing-input-response") "Please ensure the recaptcha has been completed")
                             ((string= |error| "invalid-input-secret") "Recaptcha secret is incorrect")
                             ((string= |error| "invalid-input-response") "Recaptcha response is invalid or malformed")
                             (t ""))))
    (render #p "login.html" `(:error ,error-message
                                     :sitekey ,(config :site-key)))))

(defroute ("/admin/main" :method :get) ()
  (session-protected-route (:html)
    (render-initial-page #p"adminindex.html")))

;;Query strings are read in as strings while post parameters are parsed as the type they're supposed to
(defroute ("/game/" :method :get) (&key |id|)
  (if |id|
      (handler-case (flet ((extract-if-single (suspect-list)
                             ;;This was written in light of the datafly bug I found concerning single item lists
                             (if (= (length suspect-list) 1)
                                 (car suspect-list)
                                 suspect-list)))
                      (let ((game-id (guarantee-number |id|))
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

;;POST
(defroute ("/admin/verify" :method :post) (&key |user| |password| |g-recaptcha-response|)
  (let* ((stream (http-request "https://www.google.com/recaptcha/api/siteverify"
                               :method :post
                               :parameters `(("secret" . ,(config :secret-key))
                                             ("response" . ,|g-recaptcha-response|))
                               :want-stream t))
         (google-response (progn (setf (flexi-stream-external-format stream) :utf-8)
                                 (setf google-response (decode-json stream)))))
    (if (cdr (assoc :success google-response))
        (if (and (and (stringp |user|) (> (length |user|) 0))
                 (and (stringp |password|) (> (length |password|) 0)))
            (let ((validation-sql (make-string-output-stream))
                  (sanitized-username (sanitize-string |user|))
                  (sanitized-password (sanitize-string |password|))
                  (number-login-attempts 0)
                  (is-validated nil))
              (format validation-sql "select count(*) as count from users where username='~a' and password=sha2(concat('~a', (select salt from users where username='~a')), 512);" sanitized-username sanitized-password sanitized-username)
              (with-connection (db)
                (setf number-login-attempts (getf (retrieve-one
                                                   (select :numberattempts
                                                           (from :login_attempts)
                                                           (where (:= :username sanitized-username))))
                                                  :numberattempts))
                (unless (and (realp number-login-attempts) (>= number-login-attempts 3))
                  (format t "in")
                  (progn
                    (setf is-validated (if (= 1 (getf (retrieve-one (get-output-stream-string validation-sql)) :count))
                                           t
                                           nil))
                    (if is-validated
                        (execute (delete-from :login_attempts
                                              (where (:= :username sanitized-username))))
                        (if number-login-attempts
                            (execute 
                             (update :login_attempts
                                     (set= :numberattempts (+ number-login-attempts 1))
                                     (where (:= :username sanitized-username))))
                            (execute
                             (create-insert-statement :login_attempts `(:username ,sanitized-username))))))))
              (cond ((and (realp number-login-attempts) (>= number-login-attempts 3)) (redirect "/admin?error=ELOCKED"))
                    (is-validated (progn (setf (gethash :isverified *session*) t)
                                         (redirect "/admin/main")))
                    (t (redirect "/admin?error=EINCORR"))))
            (redirect "/admin?error=EMISSING"))
        (redirect (concatenate 'string "/admin?error=" (cadr (assoc :error-codes google-response)))))))
      
(defroute ("/admin/company/" :method :post) (&key _parsed)
  (format t "~a" _parsed)
  (session-protected-route (:json)
    (add-to-table :companies '("name" "ismanufacturer") _parsed)))

(defroute ("/admin/genre/" :method :post) (&key _parsed)
  (session-protected-route (:json)
    (add-to-table :genres '("name") _parsed)))

(defroute ("/admin/system/" :method :post) (&key _parsed)
  (session-protected-route (:json)
    (add-to-table :systems '("name" "manufacturerid") _parsed)))
  

(defroute ("/admin/game/" :method :post) (&key |genres| |companies| _parsed)
  (session-protected-route (:json)
    (if (and |genres|
             |companies|
             (has-required-fields-p '("name" "region" "hasmanual" "hasbox" "quantity" "systemid") _parsed))
        (handler-case (let ((gameparameters (sanitize-input (filter-parameters _parsed '("genres" "companies"))))
                            (parsed-company-ids (parse-string-ints |companies|))
                            (parsed-genre-ids (parse-string-ints |genres|))
                            (new-game-id 'nil))
                        (with-connection (db)
                          (execute
                           (create-insert-statement :games gameparameters))
                          (setf new-game-id (getf (retrieve-one
                                                   (select :id
                                                           (from :games)
                                                           (where (:= :name (getf gameparameters :name)))))
                                                  :id))
                          (populate-pivot-tables new-game-id |genres| |companies|)
                          (render-json `(:|status| "success"  :|newid| ,new-game-id))))
          (sb-int:simple-parse-error ()
            (render-json '(:|status| "error" :|code| "ENOTINT"))))
        (render-json '(:|status| "error" :|code| "EMALFORMEDINPUT")))))


(defroute ("/search-games-ajax/" :method :post) (&key |genres| |companies| _parsed)
  ;;We sanitize the values which are then placed in an and list
  (if (or |genres|
          |companies|
          _parsed)
      (handler-case (flet ((create-or-list (id-column-name id-list)
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
                                                                         (if (symbolp (cdr parameter))
                                                                             (if (eql :false (cdr parameter))
                                                                                 `(:= ,keyword-symbol 0)
                                                                                 `(:= ,keyword-symbol 1))
                                                                             `(:= ,keyword-symbol ,(sanitize-string (cdr parameter))))))))
                                                           _parsed)))
                             (company-parameters (if |companies| (create-or-list :company_id (map 'list
                                                                                                  #'guarantee-number
                                                                                                  |companies|))))
                             (genre-parameters (if |genres| (create-or-list :genre_id (map 'list
                                                                                           #'guarantee-number
                                                                                           |genres|))))
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
                                         (inner-join :systems :on (:= :games.systemid :systems.id))
                                         (inner-join :games_genres_pivot :on (:= :games.id :games_genres_pivot.game_id ))
                                         (inner-join :genres :on (:= :games_genres_pivot.genre_id :genres.id))
                                         (inner-join :games_companies_pivot :on (:= :games_companies_pivot.game_id :games.id))
                                         (inner-join :companies :on (:= :games_companies_pivot.company_id :companies.id))
                                         (where where-arguments)
                                         (group-by :games.id)))))
                        (setf (headers *response* :content-type) "application/json")
                        (concatenate 'string "{\"results\":" (encode-json-custom result-set) "}")))
        (sb-int:simple-parse-error ()
          (render-json '(:|status| "error" :|code| "ENOTINT"))))
      (render-json `(:|status| "error" :|code| "EMALFORMEDINPUT"))))

;; PUT
(defroute ("/admin/game/" :method :put) (&key |id| |genres| |companies| _parsed)
  (session-protected-route (:json)
    (if (and |id|
             |genres|
             |companies|
             (has-required-fields-p '("name" "region" "hasmanual" "hasbox" "quantity" "systemid") _parsed))
        (handler-case (let ((game-parameters (sanitize-input (filter-parameters _parsed '("genres" "companies"))))
                            (parsed-company-ids (parse-string-ints |companies|))
                            (parsed-genre-ids (parse-string-ints |genres|))
                            (parsed-game-id (guarantee-number |id|)))
                        (with-connection (db)
                          (execute
                           (create-update-statement :games game-parameters parsed-game-id))
                          (execute
                           (delete-from :games_genres_pivot
                                        (where (:= :game_id parsed-game-id))))
                          (execute
                           (delete-from :games_companies_pivot
                                        (where (:= :game_id parsed-game-id))))
                          (populate-pivot-tables parsed-game-id parsed-genre-ids parsed-company-ids)
                          (render-json '(:|status| "success"))))
          (sb-int:simple-parse-error ()
            (render-json '(:|status| "error" :|code| "ENOTINT"))))
        (render-json '(:|status| "error" :|code| "EMALFORMEDINPUT")))))



(defroute ("/admin/system/" :method :put) (&key |id| _parsed)
  (session-protected-route (:json)
    (update-table-entry |id| :systems '("name" "manufacturerid") _parsed)))

(defroute ("/admin/company/" :method :put) (&key |id| _parsed)
  (session-protected-route (:json)
    (update-table-entry |id| :companies '("name" "ismanufacturer") _parsed)))

(defroute ("/admin/genre/" :method :put) (&key |id| _parsed)
  (session-protected-route (:json)
    (update-table-entry |id| :genres '("name") _parsed)))
                          
;; DELETE

(defroute ("/admin/game/" :method :delete) (&key |id|)
  (session-protected-route (:json)
    (if |id|
        (handler-case (let ((parsed-game-id (guarantee-number |id|)))
                        (with-connection (db)
                          (execute
                           (create-delete-statement :games parsed-game-id)))
                        (render-json '(:|status| "success")))
          (sb-int:simple-parse-error ()
            (render-json '(:|status| "error" :|code| "ENOTINT"))))
        (render-json '(:|status| "error" :|code| "EMALFORMEDINPUT")))))

(defroute ("/admin/system/" :method :delete) (&key |id|)
  (session-protected-route (:json)
    (delete-from-table |id| :systems)))

(defroute ("/admin/company/" :method :delete) (&key |id|)
  (session-protected-route (:json)
    (delete-from-table |id| :companies)))

(defroute ("/admin/genre/" :method :delete) (&key |id|)
  (session-protected-route (:json)
    (delete-from-table |id| :genres)))

;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
