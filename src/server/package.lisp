;; Common Lisp Script
;; Manoel Vilela

(defpackage #:lisp-chat/server
  (:use #:usocket
        #:cl
        #:lisp-chat/config
        #:bordeaux-threads)
  (:import-from #:websocket-driver
                #:make-server
                #:ws
                #:on
                #:send
                #:ready-state
                #:start-connection)
  (:import-from #:clack
                #:clackup
                #:stop)
  (:export #:main
           #:*clients*
           #:*messages-log*
           #:*server-nickname*
           #:client-name
           #:message-from
           #:message-time
           #:message-content
           #:get-client
           #:get-time
           #:private-message
           #:command-message
           #:send-message
           #:user-messages))

(defpackage #:lisp-chat/commands
  (:use #:cl #:lisp-chat/server)
  (:export #:get-command
           #:get-commands
           #:call-command))
