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

(defvar *uptime* (get-time) "Uptime of server variable")

(defun spacep (c)
  (eql c #\Space))

(defun split-quotation-aware (string delimiterp)
  "Split a string preserving quotation as single tokens"
  (let ((tokens nil)
        (token (make-array 10 :element-type 'character :adjustable t :fill-pointer 0))
        (quote-char nil))
    (loop for char across string
          do (cond
               ((and quote-char (char= char quote-char))
                (setf quote-char nil))
               ((and (not quote-char) (or (char= char #\") (char= char #\')))
                (setf quote-char char))
               ((and (not quote-char) (funcall delimiterp char))
                (when (plusp (length token))
                  (push (copy-seq token) tokens)
                  (setf (fill-pointer token) 0)))
               (t (vector-push-extend char token))))
    (when (plusp (length token))
      (push (copy-seq token) tokens))
    (nreverse tokens)))

(defun split (string &key (delimiterp #'spacep) quotation-aware)
  "Split a string by a delimiterp function character checking"
  (if (not quotation-aware)
      (loop for beg = (position-if-not delimiterp string)
              then (position-if-not delimiterp string :start (1+ end))
            for end = (and beg (position-if delimiterp string :start beg))
            when beg
              collect (subseq string beg end)
            while end)
      (split-quotation-aware string delimiterp)))

(defun startswith (string substring)
  "Check if STRING starts with SUBSTRING."
  (let ((l1 (length string))
        (l2 (length substring)))
    (when (and (> l2 0)
               (>= l1 l2))
      (loop for c1 across string
            for c2 across substring
            always (equal c1 c2)))))

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
  (mapcar (lambda (arg) (if (startswith arg ":")
                       (intern (string-upcase (string-left-trim ":" arg)) 'keyword)
                       arg))
          args))

(defun args-to-string (args)
  (format nil "~{~a~^ ~}" args))

(defun extract-args-as-string (string &key (accessor #'cdr))
  (args-to-string (funcall accessor (split string))))

(defun extract-params (string)
  (cdr (split string :quotation-aware t)))

(defun extract-command (string)
  (car (split string)))

(defun call-command (client message)
  (when (startswith message "/")
    (handler-case (call-command-predefined client message)
      (error (c)
        (command-message (format nil "command '~a' finished with error: ~a" message c))))))

(defun call-command-predefined (client message)
  (let* ((command-name (extract-command message))
         (command (find command-name (get-commands) :test #'string=))
         (command-function (get-command command))
         (args (extract-params message)))
    (cond
      ;; HACK(@lerax): sex 06 fev 2026 17:34:43 backward compatible with /log <n>
      ((and (string= command "/log")
            (eq (length args) 1))
       (/log client :depth (car args) :date-format "date"))
      ((string= command "/dm") (/dm client (car args) (extract-args-as-string message :accessor #'cddr)))
      ((string= command "/lisp") (/lisp client (extract-args-as-string message)))
      (command-function (apply command-function (cons client (parse-keywords args))))
      (t (command-message (format nil "command ~a doesn't exists" message))))))

(defun ensure-string (s)
  (if (stringp s)
      s
      (write-to-string s)))

;; user commands prefixed with /
(defun /search (client query &rest args &key user (limit "10") before after &allow-other-keys)
  "/search QUERY searches for messages containing QUERY as substring.
   QUERY is an mandatory parameter.
   KEY PARAMETERS:
   :user USERNAME   - Filter by a specific user.
   :limit NUMBER    - Maximum number of messages to return (default 10).
   :before ISO-DATE - ISO format datetime filter (e.g., 2026-02-22T14:30).
   :after ISO-DATE  - ISO format datetime filter (e.g., 2026-02-22T14:30)."
  (declare (ignorable client args))
  (if (not query)
      (command-message "error: QUERY is mandatory parameter. Try /search QUERY")
      (let* ((parsed-limit (parse-integer (ensure-string limit) :junk-allowed t))
             (before-time (parse-iso8601 (ensure-string before)))
             (after-time (parse-iso8601 (ensure-string after)))
             (messages *messages-log*) ;; newest first
             (filtered (remove-if-not
                        (lambda (m)
                          (and (not (equal (message-from m) "@server"))
                               (search (string-downcase query)
                                       (string-downcase (message-content m)))
                               (or (not user)
                                   (equal (string-downcase (message-from m))
                                          (string-downcase user)))
                               (or (not before-time)
                                   (<= (message-universal-time m) before-time))
                               (or (not after-time)
                                   (>= (message-universal-time m) after-time))))
                        messages))
             (limited (subseq filtered 0 (min (length filtered) parsed-limit))))
        (if (not limited)
            (command-message "the search returned a empty result")
            (format nil "~{~a~^~%~}"
                    (mapcar (lambda (m) (search-message m))
                            (reverse limited)))))))

(defun /users (client &rest args)
  "/users returns a list separated by commas of the currently logged users"
  (declare (ignorable client args))
  (command-message (format nil "users: ~{~a~^, ~}" (mapcar #'client-name *clients*))))

(defun /ping (client &rest args)
  "/ping responds with a 'pong' message, echoing the provided arguments or the user's nickname."
  (declare (ignorable client args))
  (let* ((latency (client-latency-ms client))
         (latency-msg (if latency
                          (format nil " | latency: ~,2fms" latency)
                          "")))
    (command-message (format nil "pong ~a~a" (or args (client-name client)) latency-msg))))


(defun /help (client &optional command-name &rest args)
  "/help shows a list of the available commands of lisp-chat.
   If COMMAND-NAME is provided, show its documentation."
  (declare (ignorable client args))
  (if command-name
      (let* ((cmd (if (char= (char command-name 0) #\/)
                      command-name
                      (concatenate 'string "/" command-name)))
             (sym (get-command cmd)))
        (if (and sym (fboundp sym))
            (command-message (or (documentation sym 'function)
                                 (format nil "No documentation for ~a" cmd)))
            (command-message (format nil "Command ~a not found." cmd))))
      (command-message (format nil "Available commands: ~{~a~^, ~}" (get-commands)))))

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
    (command-message (format nil "Manual of lisp-chat:~%~{~a~^~%~}~%~%~a" docs note))))

(defun /log (client &key (depth "20") (date-format nil) &allow-other-keys)
  "/log shows the last messages sent to the server.
   DEPTH is optional number of messages frames from log"
  (declare (ignorable client))
  (let* ((messages (user-messages :date-format date-format))
         (log-size (min (or (parse-integer depth :junk-allowed t) 20)
                        (length messages))))
    (format nil "~{~a~^~%~}" (reverse (subseq messages 0
                                              log-size)))))

(defun /uptime (client &rest args)
  "/uptime returns a human-readable string to preset the uptime since the server started."
  (declare (ignorable client args))
  (command-message
   (format nil "Server online since ~a" (format-time *uptime*))))

(defun /nick (client &optional (new-nick nil) &rest args)
  "/nick changes the client-name given a NEW-NICK which should be a string"
  (declare (ignorable args))
  (if new-nick
      (progn
        (push-message "@command"
                      (format nil "User @~a is now known as @~a"
                              (client-name client)
                              new-nick))
        (setf (client-name client) new-nick)
        (command-message (format nil "Your new nick is: @~a" new-nick)))
      (command-message (format nil "/nick NEW-NICKNAME"))))

(defun /dm (client &optional (username nil) msg-content)
  "/dm sends a direct message to a USERNAME"
  (let ((user (get-client username))
        (from (client-name client)))
    (cond
      ((not username) (command-message "/dm USERNAME your message"))
      ((not user) (command-message (format nil "error: ~s user not found" username)))
      ((equal from username) (command-message "you can't dm to yourself"))
      (t
       (prog1 'ignore
         (let ((msg (private-message from msg-content)))
           (send-message client msg)
           (send-message user msg)))))))

(defun /whois (client &optional (username nil))
  "/whois get basic information of a online USERNAME"
  (declare (ignorable client))
  (let ((user (get-client username)))
    (cond
      ((not username) (command-message "/whois USERNAME"))
      ((not user) (command-message (format nil "error: ~s user not found" username)))
      (t
       (let ((formatted-time (format-time (client-time user)))
             (latency (client-latency-ms user))
             (user-agent (client-user-agent user)))
         (command-message
          (format nil "User @~a at ~a (~a connection)~a, online since ~a~a"
                  (client-name user)
                  (client-address user)
                  (client-socket-type user)
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
  (command-message (format nil "lisp-chat v~a | src: ~a"
                           (lisp-chat/config:get-version)
                           lisp-chat/config:*source-code*)))

(defun /whoami (client &rest args)
  "/whoami returns information about the current client session"
  (declare (ignorable args))
  (/whois client (client-name client)))

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
      (bt:with-timeout (*lisp-command-timeout*)
        (let* ((stream (make-string-output-stream))
               (*standard-output* stream)
               (*error-output* stream))
          (isolated:read-eval-print program stream)
          (cleanup-result-program (get-output-stream-string stream))))
    (bt:timeout ()
      (format nil "TIMEOUT: Timeout occurred after ~a seconds" *lisp-command-timeout*))))

(defun /lisp (client &optional (program nil))
  "/lisp evaluates a common lisp program"
  (let ((result (execute-lisp-capture-result program)))
    (prog1 'ignore
      (push-message "@command"
                    (format nil "user @~a called lisp code `~a` ~a"
                            (client-name client) program result)))))
