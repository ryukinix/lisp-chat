(in-package :lisp-chat/server)

(defun get-remote-address (env)
  (let ((headers (getf env :headers)))
    (or (and headers (gethash "cf-connecting-ip" headers))
        (getf env :remote-addr))))

(defun ws-app (env)
  (let ((ws (make-server env))
        (client nil))
    (on :message ws
        (lambda (message)
          ;; (debug-format t "Received WS message: ~s~%" message)
          (if (null client)
              (let ((name (string-trim '(#\Space #\Return #\Newline) message)))
                (if (zerop (length name))
                    (send ws "> Name cannot be empty. Try again: ")
                    (progn
                      (setf client (make-client :name name
                                                :socket ws
                                                :address (get-remote-address env)
                                                :time (get-time)))
                      (with-lock-held (*client-lock*)
                        (push client *clients*))
                      (user-joined-message client)
                      (debug-format t "New web-socket user ~a@~a~%" name (client-address client)))))
              (let ((response (lisp-chat/commands:call-command client message)))
                (if response
                    (send-message client response)
                    (when (> (length message) 0)
                      (push-message (client-name client) message)))))))
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

(defun ping-websockets-loop ()
  (loop
    (let ((clients (with-lock-held (*client-lock*)
                     (copy-list *clients*))))
      (dolist (client clients)
        (when (equal (client-socket-type client) "WebSocket")
          (handler-case
              (let* ((sent-time (get-internal-real-time))
                     (payload-str (princ-to-string sent-time))
                     (payload (map '(vector (unsigned-byte 8)) #'char-code payload-str)))
                (websocket-driver:send-ping 
                 (client-socket client) 
                 payload
                 (lambda ()
                   (let* ((now (get-internal-real-time))
                          (rtt (* (/ (- now sent-time) internal-time-units-per-second) 1000000.0)))
                     (setf (client-connection-latency client) (round rtt))))))
            (error () nil)))))
    (sleep 5)))

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
