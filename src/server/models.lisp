(in-package :lisp-chat/server)
(defstruct message
  from content time (channel "#general"))

(defstruct client
  name socket address time
  (connection-latency nil)
  (user-agent nil)
  (active-channel "#general")
  (session-id (princ-to-string (uuid:make-v4-uuid))))

(defparameter *clients* nil "List of clients")

(defun client-socket-type (client)
  "Return the socket type for the given client."
  (typecase (client-socket client)
    (usocket:stream-usocket "TCP")
    (t "WebSocket")))

(defun get-client (client-name)
  "Get client by name"
  (find client-name
        *clients*
        :test (lambda (name client) (equal name (client-name client)))))

(defun get-client-by-session (session-id)
  (find session-id
        *clients*
        :test (lambda (sid client) (string-equal sid (client-session-id client)))))
