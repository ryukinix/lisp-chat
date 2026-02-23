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
           #:client-address
           #:client-time
           #:client-user-agent
           #:client-socket-type
           #:message-from
           #:message-time
           #:message-content
           #:make-message
           #:message-time-hour-format
           #:message-time-date-format
           #:get-client
           #:get-time
           #:format-time
           #:parse-iso8601
           #:message-universal-time
           #:search-message
           #:push-message
           #:private-message
           #:command-message
           #:send-message
           #:user-messages
           #:client-latency
           #:client-latency-ms
           #:reset-server))

(defpackage #:lisp-chat/commands
  (:use #:cl #:lisp-chat/server #:lisp-chat/config)
  (:export #:get-command
           #:get-commands
           #:call-command))
