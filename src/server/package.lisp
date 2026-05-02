;; Common Lisp Script
;; Manoel Vilela

(defpackage #:lisp-chat/server
  (:use #:cl)
  (:local-nicknames (:config :lisp-chat/config)
                    (:bt :bordeaux-threads)
                    (:usocket :usocket))
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
           #:*user-channels*
           #:*private-channels*
           #:*server-nickname*
           #:*raw-command-message*
           #:*uptime*
           #:client
           #:client-name
           #:client-address
           #:client-time
           #:client-user-agent
           #:client-socket-type
           #:client-active-channel
           #:client-session-id
           #:client-timezone
           #:client-expand-reply
           #:message
           #:message-from
           #:message-time
           #:message-content
           #:message-channel
           #:make-message
           #:message-time-hour-format
           #:message-time-date-format
           #:get-client
           #:get-time
           #:format-time
           #:formatted-message
           #:get-message-by-reference-string
           #:parse-iso8601
           #:message-universal-time
           #:search-message
           #:push-message
           #:user-joined-message
           #:user-exited-message
           #:private-message
           #:command-message
           #:send-message
           #:user-messages
           #:client-latency
           #:client-latency-ms
           #:reset-server
           #:split
           #:startswith
           #:normalize-channel))

(defpackage #:lisp-chat/commands
  (:use #:cl)
  (:local-nicknames (:server :lisp-chat/server)
                    (:config :lisp-chat/config))
  (:export #:get-command
           #:get-commands
           #:call-command))
