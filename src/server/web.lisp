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
        (let ((val (subseq channel-param 8)))
          (if (uiop:string-prefix-p "#" val) val (concatenate 'string "#" val)))
        nil)))

(defun ws-app (env)
  (let ((ws (make-server env))
        (client nil)
        (channel (parse-channel (getf env :query-string))))
    (on :message ws
        (lambda (message)
          ;; (debug-format t "Received WS message: ~s~%" message)
          (if (null client)
              (let ((name (string-trim '(#\Space #\Return #\Newline) message)))
                (if (zerop (length name))
                    (send ws "> Name cannot be empty. Try again: ")
                    (let* ((history-channel (gethash name *user-channels*))
                           (active-channel (or channel history-channel "#general")))
                      (setf client (make-client :name name
                                                :socket ws
                                                :address (get-remote-address env)
                                                :time (get-time)
                                                :user-agent (gethash "user-agent" (getf env :headers))
                                                :active-channel active-channel))
                      (setf (gethash name *user-channels*) active-channel)
                      (bt:with-lock-held (*client-lock*)
                        (push client *clients*))
                      (user-joined-message client)
                      (recalculate-client-latency client)
                      (debug-format t "New web-socket user ~a@~a~%" name (client-address client)))))
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
