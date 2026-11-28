(in-package :lisp-chat/server)

(defun get-remote-address (env)
  (let ((headers (getf env :headers)))
    (or (and headers (gethash "cf-connecting-ip" headers))
        (getf env :remote-addr))))

(defun recalculate-client-latency (client)
  (let ((ws (client-socket client)))
    (handler-case
        (let* ((sent-time (get-internal-real-time))
               (payload-str (princ-to-string sent-time))
               (payload (map '(vector (unsigned-byte 8)) #'char-code payload-str)))
          (websocket-driver:send-ping
           ws
           payload
           (lambda ()
             (let* ((now (get-internal-real-time))
                    (rtt (* (/ (- now sent-time) internal-time-units-per-second) 1000000.0)))
               (setf (client-connection-latency client) (round rtt))))))
      (error () nil))))

(defun parse-channel (query-string)
  (let* ((params (uiop:split-string (or query-string "") :separator "&"))
         (channel-param (find-if (lambda (p) (uiop:string-prefix-p "channel=" p)) params)))
    (if channel-param
        (normalize-channel (subseq channel-param 8))
        nil)))

(defun parse-tz (query-string)
  (let* ((params (uiop:split-string (or query-string "") :separator "&"))
         (tz-param (find-if (lambda (p) (uiop:string-prefix-p "tz=" p)) params)))
    (if tz-param
        (handler-case (- (parse-integer (subseq tz-param 3)))
          (error () nil))
        nil)))

(defun parse-expand-reply (query-string)
  (let* ((params (uiop:split-string (or query-string "") :separator "&"))
         (expand-param (find-if (lambda (p) (uiop:string-prefix-p "expand_reply=" p)) params)))
    (if expand-param
        (let ((val (subseq expand-param 13)))
          (not (string-equal val "false")))
        t)))

(defun parse-session-id (query-string)
  (let* ((params (uiop:split-string (or query-string "") :separator "&"))
         (param (find-if (lambda (p) (uiop:string-prefix-p "session_id=" p)) params)))
    (when param (subseq param 11))))

(defun ws-app (env)
  (let* ((ws (make-server env))
         (client nil)
         (query-string (getf env :query-string))
         (channel (parse-channel query-string))
         (tz (parse-tz query-string))
         (expand-reply (parse-expand-reply query-string))
         (client-session-id (or (parse-session-id query-string)
                                (princ-to-string (uuid:make-v4-uuid)))))
    (on :message ws
        (lambda (message)
          ;; (debug-format t "Received WS message: ~s~%" message)
          (if (null client)
              (multiple-value-bind (name modified-p) (normalize-username message)
                (if (zerop (length name))
                    (send ws "> Name cannot be empty. Try again: ")
                    (let* ((history-channel (gethash name *user-channels*))
                           (active-channel (or channel history-channel "#general")))
                      (setf client (make-client :name name
                                                :socket ws
                                                :address (get-remote-address env)
                                                :time (get-time)
                                                :user-agent (gethash "user-agent" (getf env :headers))
                                                :active-channel active-channel
                                                :timezone tz
                                                :expand-reply expand-reply
                                                :session-id client-session-id))
                      (setf (gethash name *user-channels*) active-channel)
                      (register-user-session name client-session-id)
                      (bt:with-lock-held (*client-lock*)
                        (push client *clients*))
                      (when modified-p
                        (send-message client
                                      (command-message (format nil "Your nickname was normalized to: @~a" name)
                                                       :client client)))
                      (user-joined-message client)
                      (recalculate-client-latency client)
                      (debug-format t "New web-socket user ~a@~a (session ~a)~%"
                                    name (client-address client) client-session-id))))
              (let ((response (lisp-chat/commands:call-command client message)))
                (if response
                    (send-message client response)
                    (when (> (length message) 0)
                      (push-message (client-name client) message :channel (client-active-channel client))))))
          (recalculate-client-latency client)))
    (on :open ws
        (lambda ()
          (send ws "> Type your username: ")))
    (on :close ws
        (lambda (&key code reason)
          (declare (ignore code reason))
          (when client
            (client-delete client))))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defvar *web-app* (make-instance 'ningle:app))

(setf (ningle:route *web-app* "/api/commands/:command" :method :OPTIONS)
      (lambda (params)
        (let ((command-name (cdr (assoc :command params)))
              (env (lack.request:request-env ningle:*request*)))
          (api-options-app env (concatenate 'string "/" command-name)))))

(setf (ningle:route *web-app* "/api/commands/:command" :method :POST)
      (lambda (params)
        (let ((command-name (cdr (assoc :command params)))
              (env (lack.request:request-env ningle:*request*)))
          (api-app env (concatenate 'string "/" command-name)))))

(setf (ningle:route *web-app* "/api/notifications/vapid-public-key" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (if *vapid-public-key*
            `(200 (:content-type "text/plain"
                   :cache-control "no-store"
                   :access-control-allow-origin "*")
                  (,*vapid-public-key*))
            (json-response 503 '(:error "VAPID keys not initialized")))))

(setf (ningle:route *web-app* "/api/notifications/subscribe-push" :method :POST)
      (lambda (params)
        (let* ((content (lack.request:request-content ningle:*request*))
               (raw-json (when (and content (plusp (length content)))
                           (babel:octets-to-string content :encoding :utf-8)))
               (session-id (cdr (assoc "session_id" params :test #'string-equal))))
          (cond
            ((or (not session-id) (zerop (length session-id)) (not raw-json))
             (json-response 400 '(:error "session_id parameter and JSON body are required")))
            ((not (extract-subscription-endpoint raw-json))
             (json-response 400 '(:error "Invalid push subscription JSON: missing endpoint")))
            (t
             (subscribe-push-session session-id raw-json)
             (json-response 200 '(:success t)))))))

(setf (ningle:route *web-app* "/api/notifications/unsubscribe-push" :method :POST)
      (lambda (params)
        (let* ((content (lack.request:request-content ningle:*request*))
               (raw-json (when (and content (plusp (length content)))
                           (babel:octets-to-string content :encoding :utf-8)))
               (endpoint (when raw-json (extract-subscription-endpoint raw-json)))
               (session-id (cdr (assoc "session_id" params :test #'string-equal))))
          (if (or (not session-id) (zerop (length session-id)))
              (json-response 400 '(:error "session_id parameter is required"))
              (progn
                (unsubscribe-push-session session-id endpoint)
                (json-response 200 '(:success t)))))))

(setf (ningle:route *web-app* "/ws")
      (lambda (params)
        (declare (ignore params))
        (ws-app (lack.request:request-env ningle:*request*))))

(setf (ningle:route *web-app* "/")
      (lambda (params)
        (declare (ignore params))
        `(200 (:content-type "text/html"
               :cache-control "no-store, no-cache, must-revalidate, max-age=0"
               :pragma "no-cache"
               :expires "0")
              ,(pathname (merge-pathnames "src/static/index.html" (asdf:system-source-directory :lisp-chat))))))

(defparameter *app*
  (lack:builder
   (:static :path (lambda (path)
                    (if (or (string= path "/")
                            (uiop:string-prefix-p "/api/" path)
                            (string= path "/ws"))
                        nil
                        path))
            :root (merge-pathnames "src/static/" (asdf:system-source-directory :lisp-chat)))
   *web-app*))
