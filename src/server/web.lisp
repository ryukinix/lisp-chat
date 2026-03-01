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
                      (with-lock-held (*client-lock*)
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

(defun static-response-headers (content-type)
  (let ((no-cache '("text/html")))
    (cond
      ((find content-type no-cache)
       `(:content-type ,content-type
        :cache-control "no-store, no-cache, must-revalidate, max-age=0"
        :pragma "no-cache"
        :expires "0"))
      (t `(:content-type ,content-type)))))

(defun static-app (path)
  (let* ((file (merge-pathnames (subseq path 1) (merge-pathnames "src/static/"
                                                                (asdf:system-source-directory :lisp-chat))))
         (extension (pathname-type file))
         (content-type (cond
                         ((string= extension "html") "text/html")
                         ((string= extension "css") "text/css")
                         ((string= extension "js") "application/javascript")
                         ((string= extension "json") "application/json")
                         ((string= extension "png") "image/png")
                         ((string= extension "ico") "image/x-icon")
                         (t "text/plain"))))
    (if (probe-file file)
        `(200 ,(static-response-headers content-type) ,file)
        '(404 (:content-type "text/plain") ("Not Found")))))

(defun web-handler (env)
  (let ((path (getf env :path-info)))
    (cond
      ((string= path "/ws")
       (ws-app env))
      ((string= path "/")
       (static-app "/index.html"))
      (t
       (static-app path)))))
