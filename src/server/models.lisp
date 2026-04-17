(in-package :lisp-chat/server)
(export '(message client make-message message-from message-content message-time message-channel
          make-client client-name client-socket client-address client-time client-active-channel client-session-id))

(defstruct message
  from content time (channel "#general"))

(defstruct client
  name socket address time
  (connection-latency nil)
  (user-agent nil)
  (active-channel "#general")
  (session-id (princ-to-string (uuid:make-v4-uuid))))
