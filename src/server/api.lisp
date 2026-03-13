(in-package :lisp-chat/server)

(defparameter *api-unauthenticated-commands*
  '("/version" "/uptime" "/channels" "/users" "/help" "/man" "/search" "/log" "/whois"))

(defparameter *api-blocked-commands*
  '("/lisp" "/session"))

(defparameter *api-formatted-commands*
  '("/search" "/log"))

(defun read-stream-to-string (stream)
  (with-output-to-string (s)
    (loop for char = (read-char stream nil nil)
          while char
          do (write-char char s))))

(defun parse-json-body (env)
  (let ((raw-body (getf env :raw-body)))
    (if raw-body
        (handler-case
            (let ((body-str (read-stream-to-string raw-body)))
              (if (zerop (length body-str))
                  (make-hash-table)
                  (yason:parse body-str)))
          (error (e)
            (declare (ignore e))
            :error))
        (make-hash-table))))

(defun plist-to-json (plist)
  (let ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)
        (yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase))
      (with-output-to-string (s)
        (yason:encode-plist plist s))))

(defun json-response (code plist)
  `(,code (:content-type "application/json")
          (,(plist-to-json plist))))

(defun api-app (env command-name)
  (let* ((command-sym (lisp-chat/commands:get-command command-name))
         (headers (getf env :headers))
         (session-id (and headers (gethash "client-session" headers)))
         (is-unauthenticated (member command-name *api-unauthenticated-commands* :test #'string-equal))
         (is-blocked (member command-name *api-blocked-commands* :test #'string-equal))
         (client (if session-id
                     (get-client-by-session session-id)
                     (when is-unauthenticated
                       (make-client :name "api"
                                    :active-channel "#general"
                                    :address (get-remote-address env))))))
    (cond
      ((not command-sym) (json-response 404 '(:error "Command not found")))
      ((not client) (json-response 401 '(:error "Unauthorized: valid Client-Session header required")))
      (is-blocked (json-response 403 '(:error "Not allowed: command is not available to use through API")))
      (t
       (let ((payload (parse-json-body env)))
         (if (eq payload :error)
             `(400 (:content-type "application/json") ("{\"error\": \"Invalid JSON in request body\"}"))
             (let* ((args (gethash "args" payload nil))
                    (kwargs-hash (gethash "kwargs" payload nil))
                    (channel-override (gethash "channel" payload nil))
                    (active-client (if channel-override
                                       (let ((new-client (copy-client client)))
                                         (setf (client-active-channel new-client) channel-override)
                                         new-client)
                                       client))
                    (kwargs (when kwargs-hash
                              (loop for k being the hash-keys of kwargs-hash using (hash-value v)
                                    append (list (intern (string-upcase k) "KEYWORD") v))))
                    (result (handler-case
                                (let ((*raw-command-message* (not (member command-name
                                                                          *api-formatted-commands*
                                                                          :test #'string-equal))))
                                  (apply command-sym active-client (append args kwargs)))
                              (error (e)
                                (format nil "Error executing command: ~A" e)))))
               (json-response 200 `(:result ,(format nil "~A" result))))))))))

(defun api-options-app (env command-name)
  (declare (ignore env))
  (let ((command-sym (lisp-chat/commands:get-command command-name)))
    (if (not command-sym)
        `(404 (:content-type "text/plain") ("Command not found"))
        (let ((description (with-output-to-string (s)
                             (describe command-sym s))))
          `(200 (:content-type "text/plain") (,description))))))
