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
           #:*vapid-public-key*
           #:*vapid-private-key*
           #:*vapid-subject*
           #:*push-subscriptions*
           #:*username-to-sessions*
           #:*username-to-sessions-lock*
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
           #:push-notification
           #:register-user-session
           #:subscribe-push-session
           #:unsubscribe-push-session
           #:load-or-create-vapid-keys
           #:load-push-subscriptions
           #:save-push-subscriptions
           #:load-user-sessions
           #:save-user-sessions
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
           #:extract-mentions
           #:extract-reply-targets
           #:extract-notification-targets
           #:startswith
           #:normalize-channel
           #:normalize-username))

(defpackage #:lisp-chat/commands
  (:use #:cl)
  (:local-nicknames (:server :lisp-chat/server)
                    (:config :lisp-chat/config))
  (:export #:get-command
           #:get-commands
           #:call-command))
