(in-package :lisp-chat/server)

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
                                                :address (getf env :remote-addr)))
                      (with-lock-held (*client-lock*)
                        (push client *clients*))
                      (push-message "@server" (format nil "The user ~s joined to the party!" name))
                      (debug-format t "New web-socket user ~a@~a~%" name (client-address client)))))
              (let ((response (call-command client message)))
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
        `(200 (:content-type ,content-type) ,file)
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
