(defpackage :lisp-chat/tests
  (:use :cl :cl-user :parachute)
  (:import-from :lisp-chat/config :*port* :*websocket-port* :*host* :*debug* :*lisp-command-timeout*)
  (:import-from :usocket :socket-connect :socket-close :socket-stream)
  (:import-from :websocket-driver :start-connection :send :on :close-connection)
  (:import-from :websocket-driver-client :make-client)
  (:export :run-tests))
