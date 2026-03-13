(in-package :lisp-chat/tests)

(define-test api-tests
  :parent lisp-chat-tests)

(defun api-url (path)
  (format nil "http://127.0.0.1:~a~a" config:*websocket-port* path))

(define-test unauthenticated-api-commands
  :parent api-tests
  (multiple-value-bind (body status)
      (dex:post (api-url "/api/commands/version")
                :content "{}"
                :headers '(("Content-Type" . "application/json")))
    (is = 200 status)
    (let ((data (yason:parse body)))
      (true (gethash "result" data))
      (true (search "lisp-chat" (gethash "result" data))
            (format nil "Expected 'lisp-chat' in result, got: ~A" (gethash "result" data))))))

(define-test unauthorized-api-commands
  :parent api-tests
  (let ((url (api-url "/api/commands/join")))
    (handler-case
        (dex:post url :content "{\"args\": [\"#lisp\"]}" :headers '(("Content-Type" . "application/json")))
      (dex:http-request-failed (e)
        (is = 401 (dex:response-status e))
        (let ((data (yason:parse (dex:response-body e))))
          (is string= "Unauthorized: valid Client-Session header required" (gethash "error" data)))))))

(define-test invalid-json-api-commands
  :parent api-tests
  (let ((url (api-url "/api/commands/version")))
    (handler-case
        (dex:post url :content "{\"invalid json" :headers '(("Content-Type" . "application/json")))
      (dex:http-request-failed (e)
        (is = 400 (dex:response-status e))))))

(define-test authenticated-api-commands
  :parent api-tests
  (with-websocket-client (client messages)
    (ws-interaction client (lambda () messages)
      '(:expect "Type your username")
      "api-tester"
      '(:expect "The user @api-tester joined to the party!")
      '(:sleep 0.2))
    (let* ((active-client (lisp-chat/server::get-client "api-tester"))
           (session-id (lisp-chat/server::client-session-id active-client)))
      (multiple-value-bind (body status)
          (dex:post (api-url "/api/commands/uptime")
                    :content "{}"
                    :headers `(("Content-Type" . "application/json")
                               ("Client-Session" . ,session-id)))
        (is = 200 status)
        (let ((data (yason:parse body)))
          (true (gethash "result" data))
          (true (search "Server online since" (gethash "result" data))
                (format nil "Expected 'Server online since' in result, got: ~A" (gethash "result" data))))))))

(define-test authenticated-api-state-change
  :parent api-tests
  (with-websocket-client (client messages)
    (ws-interaction client (lambda () messages)
      '(:expect "Type your username")
      "api-state-tester"
      '(:expect "The user @api-state-tester joined to the party!")
      '(:sleep 0.2))
    (let* ((active-client (lisp-chat/server::get-client "api-state-tester"))
           (session-id (lisp-chat/server::client-session-id active-client)))
      (multiple-value-bind (body status)
          (dex:post (api-url "/api/commands/join")
                    :content "{\"args\": [\"#new-channel\"]}"
                    :headers `(("Content-Type" . "application/json")
                               ("Client-Session" . ,session-id)))
        (declare (ignorable body))
        (is = 200 status)
        ;; Verify the client actually changed channel
        (is string= "#new-channel" (lisp-chat/server::client-active-channel active-client))))))

(define-test log-api-kwargs
  :parent api-tests
  (let ((current-date (get-current-date)))
    (multiple-value-bind (body status)
        (dex:post (api-url "/api/commands/log")
                  :content "{\"kwargs\": {\"date-format\": \"date\"}}"
                  :headers '(("Content-Type" . "application/json")))
      (is = 200 status)
      (let* ((data (yason:parse body))
             (result (gethash "result" data)))
        (true result)
        (true (search current-date result)
              (format nil "Expected current date ~A in result, got: ~A" current-date result))))))
