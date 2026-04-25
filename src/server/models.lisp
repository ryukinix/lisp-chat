(in-package :lisp-chat/server)

(defparameter *clients* nil "List of clients")

(defstruct message
  "This structure abstract the type message with is saved
   into *messages-log* and until consumed, temporally pushed
   to *messages-stack*. FROM and CONTENT has type string, TIME is a list of decoded time parts."
  from
  content
  time
  (channel "#general"))

(defstruct client
  "This structure handle the creation/control of the clients of the server.
   NAME is a string. Socket is a USOCKET:SOCKET and address is a ipv4 encoded
   string. TIME is a list of decoded time parts since the users is online."
  name
  socket
  address
  time
  (connection-latency nil)
  (user-agent nil)
  (active-channel "#general")
  (timezone nil)
  (session-id (princ-to-string (uuid:make-v4-uuid))))

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
