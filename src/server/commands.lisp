(in-package :lisp-chat/commands)


;; Enable standard CL symbols in cl-isolated environment
;; By default cl-isolated disables many features for security reasons.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun enable-isolated-symbols (symbols)
    (dolist (symbol symbols)
      (shadowing-import symbol :isolated-cl)
      (export symbol :isolated-cl)
      (setf (get symbol :isolated-locked) nil)))
  (enable-isolated-symbols '(cl:loop
                             cl:macroexpand
                             cl:macroexpand-1)))

(defun get-commands ()
  "Returns a list of all available command strings."
  (let ((commands '()))
    (do-symbols (s (find-package :lisp-chat/commands))
      (when (and (eq (symbol-package s) (find-package :lisp-chat/commands))
                 (fboundp s)
                 (char= (char (symbol-name s) 0) #\/))
        (push (string-downcase (symbol-name s)) commands)))
    (sort commands #'string<)))

(defun get-command (command-string)
  "Returns the function symbol for a given command string."
  (find-symbol (string-upcase command-string) :lisp-chat/commands))

(defun parse-keywords (args)
  (mapcar (lambda (arg) (if (server:startswith arg ":")
                       (intern (string-upcase (string-left-trim ":" arg)) 'keyword)
                       arg))
          args))

(defun args-to-string (args)
  (format nil "~{~a~^ ~}" args))

(defun extract-args-as-string (string &key (accessor #'cdr))
  (args-to-string (funcall accessor (server:split string))))

(defun extract-params (string)
  (cdr (server:split string :quotation-aware t)))

(defun extract-command (string)
  (car (server:split string)))

(defun call-command (client message)
  "Call a command from the given message."
  (when (server:startswith message "/")
    (handler-case (call-command-predefined client message)
      (error (c)
        (server:command-message (format nil "command '~a' finished with error: ~a" message c))))))

(defun call-command-predefined (client message)
  (let* ((command-name (extract-command message))
         (command (find command-name (get-commands) :test #'string-equal))
         (command-function (get-command command))
         (args (extract-params message)))
    (cond
      ;; HACK(@lerax): sex 06 fev 2026 17:34:43 backward compatible with /log <n>
      ((and (string-equal command "/log")
            (eq (length args) 1))
       (/log client :depth (car args) :date-format "date"))
      ((and (string-equal command "/users")
            (eq (length args) 1))
       (/users client :channel (car args)))
      ((string-equal command "/dm") (/dm client (car args) (extract-args-as-string message :accessor #'cddr)))
      ((string-equal command "/lisp") (/lisp client (extract-args-as-string message)))
      (command-function (apply command-function (cons client (parse-keywords args))))
      (t (server:command-message (format nil "command ~a doesn't exists" message))))))

(defun ensure-string (s)
  (if (stringp s)
      s
      (write-to-string s)))

;; user commands prefixed with /
(defun /search (client query &rest args &key user (limit "10") before after (global nil) &allow-other-keys)
  "/search QUERY searches for messages containing QUERY as substring.
   QUERY is an mandatory parameter.
   KEY PARAMETERS:
   :user USERNAME   - Filter by a specific user.
   :limit NUMBER    - Maximum number of messages to return (default 10).
   :before ISO-DATE - ISO format datetime filter (e.g., 2026-02-22T14:30).
   :after ISO-DATE  - ISO format datetime filter (e.g., 2026-02-22T14:30).
   :global BOOLEAN  - Search across all channels."
  (declare (ignorable client args))
  (if (not query)
      (server:command-message "error: QUERY is mandatory parameter. Try /search QUERY")
      (let* ((parsed-limit (parse-integer (ensure-string limit) :junk-allowed t))
             (before-time (server:parse-iso8601 (ensure-string before)))
             (after-time (server:parse-iso8601 (ensure-string after)))
             (messages server:*messages-log*) ;; newest first
             (filtered (remove-if-not
                        (lambda (m)
                          (and (not (equal (server:message-from m) "@server"))
                               (or global
                                   (string-equal (server:message-channel m) (server:client-active-channel client)))
                               (search query
                                       (server:message-content m)
                                       :test #'char-equal)
                               (or (not user)
                                   (string-equal (server:message-from m)
                                          user))
                               (or (not before-time)
                                   (<= (server:message-universal-time m) before-time))
                               (or (not after-time)
                                   (>= (server:message-universal-time m) after-time))))
                        messages))
             (limited (subseq filtered 0 (min (length filtered) parsed-limit))))
        (if (not limited)
            (server:command-message "the search returned a empty result")
            (format nil "~{~a~^~%~}"
                    (mapcar (lambda (m) (server:search-message m :global global))
                            (reverse limited)))))))

(defun /users (client &key (channel nil) (global nil) &allow-other-keys)
  "/users returns a list separated by commas of the currently logged users.
   If CHANNEL is provided, show users in that channel.
   If GLOBAL is provided, show users in all channels."
  (let* ((target-channel (if channel
                             (server:normalize-channel channel)
                             (server:client-active-channel client)))
         (channel-users (if global
                            server:*clients*
                            (remove-if-not (lambda (c) (string-equal (server:client-active-channel c)
                                                               target-channel))
                                           server:*clients*))))
    (server:command-message (format nil "users: ~{~a~^, ~}" (mapcar #'server:client-name channel-users)))))

(defun /join (client &optional (channel nil) &rest args)
  "/join changes the active channel for the user"
  (declare (ignorable args))
  (if channel
      (let ((new-channel (server:normalize-channel channel))
            (old-channel (server:client-active-channel client)))
        (if (string-equal new-channel old-channel)
            (server:command-message (format nil "You are already in ~a" new-channel))
            (prog1 'ignore
              (server:user-exited-message client)
              (setf (server:client-active-channel client) new-channel)
              (setf (gethash (server:client-name client) server:*user-channels*) new-channel)
              (server:user-joined-message client))))
      (server:command-message "/join #CHANNEL-NAME")))

(defun /channels (client &rest args &key (usernames nil) (all nil) &allow-other-keys)
  "/channels lists active channels and their user counts.
   If :usernames t is provided, lists usernames separated by commas instead.
   If :all t is provided, includes channels with no active users but have message history."
  (declare (ignorable client args))
  (let ((chan-users (make-hash-table :test 'equal)))
    (setf (gethash "#general" chan-users) nil)
    (when all
      (loop for m in server:*messages-log*
            do (unless (nth-value 1 (gethash (server:message-channel m) chan-users))
                 (setf (gethash (server:message-channel m) chan-users) nil))))
    (loop for c in server:*clients*
          do (push (server:client-name c) (gethash (server:client-active-channel c) chan-users nil)))
    (let ((lines nil))
      (maphash (lambda (chan users)
                 (unless (gethash chan server:*private-channels*)
                   (if usernames
                       (push (if users
                                 (format nil "~a: ~{~a~^, ~}" chan (reverse users))
                                 (format nil "~a: 0 users" chan))
                             lines)
                       (push (format nil "~a: ~a user~:p" chan (length users)) lines))))
               chan-users)
      (server:command-message (format nil "channels:~%~{~a~^~%~}" (sort lines #'string<))))))

(defun /private (client &optional (action nil) &rest args)
  "/private toggles or sets the private mode for the current channel.
   When active, messages are not saved to disk or history.
   Usage: /private [on|off|status]"
  (declare (ignorable args))
  (let ((channel (server:client-active-channel client)))
    (if (string-equal channel "#general")
        (server:command-message "This mode cannot be activated in the #general channel.")
        (let ((is-private (gethash channel server:*private-channels*)))
          (cond
            ((string-equal action "status")
             (server:command-message (format nil "Private mode for ~a is currently ~:[OFF~;ON~]." channel is-private)))
            ((or (null action) (string-equal action "on") (string-equal action "off"))
             (let ((turn-on (cond
                              ((string-equal action "on") t)
                              ((string-equal action "off") nil)
                              (t (not is-private)))))
               (if (eq (not (null is-private)) turn-on)
                   (server:command-message (format nil "Private mode is already ~:[OFF~;ON~]." turn-on))
                   (prog1 'ignore
                     (setf (gethash channel server:*private-channels*) turn-on)
                     (server:push-message "@server"
                                   (format nil "Private mode was ~:[deactivated~;activated~] by @~a"
                                           turn-on (server:client-name client))
                                   :channel channel)))))
            (t
             (server:command-message "Usage: /private [on|off|status]")))))))

(defun /ping (client &rest args)
  "/ping responds with a 'pong' message, echoing the provided arguments or the user's nickname."
  (declare (ignorable client args))
  (let* ((latency (server:client-latency-ms client))
         (latency-msg (if latency
                          (format nil " | latency: ~,2fms" latency)
                          "")))
    (server:command-message (format nil "pong ~a~a" (or args (server:client-name client)) latency-msg))))


(defun /help (client &optional command-name &rest args)
  "/help shows a list of the available commands of lisp-chat.
   If COMMAND-NAME is provided, show its documentation."
  (declare (ignorable client args))
  (if command-name
      (let* ((cmd (if (server:startswith command-name "/")
                      command-name
                      (concatenate 'string "/" command-name)))
             (sym (get-command cmd)))
        (if (and sym (fboundp sym))
            (server:command-message (or (documentation sym 'function)
                                 (format nil "No documentation for ~a" cmd)))
            (server:command-message (format nil "Command ~a not found." cmd))))
      (server:command-message (format nil "Available commands: ~{~a~^, ~}" (get-commands)))))

(defun /man (client &rest args)
  "/man shows the docstrings of all available commands"
  (declare (ignorable client args))
  (let* ((commands (get-commands))
         (docs (mapcar (lambda (cmd)
                         (let* ((sym (get-command cmd))
                                (doc (when (and sym (fboundp sym))
                                       (documentation sym 'function))))
                           (if doc
                               doc
                               (format nil "~a: No documentation" cmd))))
                       commands))
         (note "Note: /clear and /quit are front-end commands, depending the implementation of the client."))
    (server:command-message (format nil "Manual of lisp-chat:~%~{~a~^~%~}~%~%~a" docs note))))

(defun /log (client &key (depth "20") (date-format nil) (global nil) &allow-other-keys)
  "/log shows the last messages sent to the server.
   DEPTH is optional number of messages frames from log
   GLOBAL is optional boolean to search across all channels"
  (declare (ignorable client))
  (let* ((messages (server:user-messages :date-format date-format
                                         :channel (server:client-active-channel client)
                                         :global global))
         (log-size (min (or (parse-integer depth :junk-allowed t) 20)
                        (length messages))))
    (format nil "~{~a~^~%~}" (reverse (subseq messages 0
                                              log-size)))))

(defun /uptime (client &rest args)
  "/uptime returns a human-readable string to preset the uptime since the server started."
  (declare (ignorable client args))
  (server:command-message
   (format nil "Server online since ~a" (server:format-time server:*uptime*))))

(defun /session (client &rest args)
  "/session returns the unique session UUID for the current client."
  (declare (ignorable args))
  (server:command-message (format nil "Your session ID is: ~A" (server:client-session-id client))))

(defun /nick (client &optional (new-nick nil) &rest args)
  "/nick changes the server:client-name given a NEW-NICK which should be a string"
  (declare (ignorable args))
  (if new-nick
      (progn
        (server:push-message "@command"
                      (format nil "User @~a is now known as @~a"
                              (server:client-name client)
                              new-nick)
                      :channel (server:client-active-channel client))
        (setf (server:client-name client) new-nick)
        (server:command-message (format nil "Your new nick is: @~a" new-nick)))
      (server:command-message (format nil "/nick NEW-NICKNAME"))))

(defun /dm (client &optional (username nil) msg-content)
  "/dm sends a direct message to a USERNAME"
  (let ((user (server:get-client username))
        (from (server:client-name client)))
    (cond
      ((not username) (server:command-message "/dm USERNAME your message"))
      ((not user) (server:command-message (format nil "error: ~s user not found" username)))
      ((string= from username) (server:command-message "you can't dm to yourself"))
      (t
       (prog1 'ignore
         (let ((msg (server:private-message from msg-content)))
           (server:send-message client msg)
           (server:send-message user msg)))))))

(defun /whois (client &optional (username nil))
  "/whois get basic information of a online USERNAME"
  (declare (ignorable client))
  (let ((user (server:get-client username)))
    (cond
      ((not username) (server:command-message "/whois USERNAME"))
      ((not user) (server:command-message (format nil "error: ~s user not found" username)))
      (t
       (let ((formatted-time (server:format-time (server:client-time user)))
             (latency (server:client-latency-ms user))
             (user-agent (server:client-user-agent user))
             (channel (server:client-active-channel user)))
         (server:command-message
          (format nil "User @~a at ~a (~a connection) in ~a~a, online since ~a~a"
                  (server:client-name user)
                  (server:client-address user)
                  (server:client-socket-type user)
                  channel
                  (if latency
                      (format nil " with latency of ~,2fms" latency)
                      "")
                  formatted-time
                  (if user-agent
                      (format nil " | user-agent: ~a" user-agent)
                      ""))))))))
(defun /version (client &rest args)
  "/version returns the current version of lisp-chat"
  (declare (ignorable client args))
  (server:command-message (format nil "lisp-chat v~a | src: ~a"
                           (lisp-chat/config:get-version)
                           lisp-chat/config:*source-code*)))

(defun /whoami (client &rest args)
  "/whoami returns information about the current client session"
  (declare (ignorable args))
  (/whois client (server:client-name client)))

(defun /clear (client &rest args)
  "/clear clears the terminal screen"
  (declare (ignorable client args))
  'ignore)

(defun /quit (client &rest args)
  "/quit terminates the connection"
  (declare (ignorable client args))
  'ignore)

(defun cleanup-result-program (result)
  (string-trim '(#\Space #\Newline #\Return #\Tab #\Linefeed) result))

(defun execute-lisp-capture-result (program)
  (handler-case
      (bt:with-timeout (config:*lisp-command-timeout*)
        (let* ((stream (make-string-output-stream))
               (*standard-output* stream)
               (*error-output* stream))
          (isolated:read-eval-print program stream)
          (cleanup-result-program (get-output-stream-string stream))))
    (bt:timeout ()
      (format nil "TIMEOUT: Timeout occurred after ~a seconds" config:*lisp-command-timeout*))))

(defun /lisp (client &optional (program nil))
  "/lisp evaluates a common lisp program"
  (let ((result (execute-lisp-capture-result program)))
    (prog1 'ignore
      (server:push-message "@command"
                    (format nil "user @~a called lisp code `~a` ~a"
                            (server:client-name client) program result)
                    :channel (server:client-active-channel client)))))
