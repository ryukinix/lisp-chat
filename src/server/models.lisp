(in-package :lisp-chat/server)
(defstruct message
  from content time (channel "#general"))

(defstruct client
  name socket address time
  (connection-latency nil)
  (user-agent nil)
  (active-channel "#general")
  (session-id (princ-to-string (uuid:make-v4-uuid))))
