(in-package :lisp-chat/commands)

;; global vars
(defvar *day-names* '("Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday" "Saturday" "Sunday")
  "Day names")
(defvar *uptime* (multiple-value-list (get-decoded-time))
  "Uptime of server variable")

(defun split (string delimiterp)
  "Split a string by a delimiterp function character checking"
  (loop for beg = (position-if-not delimiterp string)
          then (position-if-not delimiterp string :start (1+ end))
        for end = (and beg (position-if delimiterp string :start beg))
        when beg
          collect (subseq string beg end)
        while end))

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

(defun extract-params (string)
  (let ((args (subseq (split string (lambda (c) (eql c #\Space))) 1)))
    (parse-keywords args)))

(defun call-command (client message)
  (when (startswith message "/")
    (handler-case (call-command-predefined client message)
      (error (c)
        (command-message (format nil "command '~a' finished with error: ~a" message c))))))

(defun call-command-predefined (client message)
  (let ((command (find message (get-commands) :test #'startswith)))
    (let ((command-function (get-command command))
          (args (extract-params message)))
      (cond
        ;; HACK(@lerax): sex 06 fev 2026 17:34:43 backward compatible with /log <n>
        ((and (string= command "/log")
              (eq (length args) 1))
         (/log client :depth (car args)))
        ((string= command "/dm") (/dm client (car args) (args-to-string (cdr args))))
        ((string= command "/lisp") (/lisp client (args-to-string args)))
        (command-function (apply command-function (cons client args)))
        (t (command-message (format nil "command ~a doesn't exists" message)))))))

(defun args-to-string (args)
  (format nil "~{~a~^ ~}" args))

;; user commands prefixed with /
(defun /users (client &rest args)
  "/users returns a list separated by commas of the currently logged users"
  (declare (ignorable client args))
  (command-message (format nil "users: ~{~a~^, ~}" (mapcar #'client-name *clients*))))

(defun /ping (client &rest args)
  "/ping returns a list separated by commas of the currently logged users"
  (declare (ignorable client args))
  (command-message (format nil "pong ~a" (or args (client-name client)))))


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

(defun /log (client &key (depth "20") (date-format nil) &allow-other-keys)
  "/log shows the last messages sent to the server.
   DEPTH is optional number of messages frames from log"
  (declare (ignorable client args))
  (let* ((messages (user-messages :date-format date-format))
         (log-size (min (or (parse-integer depth :junk-allowed t) 20)
                        (length messages))))
    (format nil "~{~a~^~%~}" (reverse (subseq messages 0
                                              log-size)))))

(defun /uptime (client &rest args)
  "/uptime returns a human-readable string to preset the uptime since the server started."
  (declare (ignorable client args))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (values-list *uptime*)
    (declare (ignore dst-p))
    (command-message
     (format nil
             "Server online since ~2,'0d:~2,'0d:~2,'0d of ~a, ~4,'0d-~2,'0d-~2,'0d (GMT~@d)"
             hour minute second
             (nth day-of-week *day-names*)
             year month date
             (- tz)))))

(defun /nick (client &optional (new-nick nil) &rest args)
  "/nick changes the client-name given a NEW-NICK which should be a string"
  (declare (ignorable args))
  (if new-nick
      (progn (setf (client-name client) new-nick)
             (command-message (format nil "Your new nick is: ~a" new-nick)))
      (command-message (format nil "/nick <new-nickname>"))))

(defun /dm (client &optional (username nil) msg-content)
  "/dm sends a direct message to a USERNAME"
  (let ((user (get-client username))
        (from (client-name client)))
    (cond
      ((not username) (command-message "/dm <username> your message"))
      ((not user) (command-message (format nil "'~a' user not found" username)))
      ((equal from username) (command-message "you can't dm to yourself"))
      (t
       (prog1 'ignore
         (let ((msg (private-message from msg-content)))
           (send-message client msg)
           (send-message user msg)))))))

(defun /version (client &rest args)
  "/version returns the current version of lisp-chat"
  (declare (ignorable client args))
  (command-message (format nil "lisp-chat v~a"
                           (lisp-chat/config:get-version))))

(defun cleanup-result-program (result)
  (string-trim '(#\Space #\Newline #\Return #\Tab #\Linefeed) result))

(defun execute-lisp-capture-result (program)
  (let* ((stream (make-string-output-stream))
         (*standard-output* stream)
         (*error-output* stream))
    (isolated:read-eval-print program stream)
    (cleanup-result-program (get-output-stream-string stream))))

(defun /lisp (client &optional (program nil))
  "/lisp evaluates a common lisp program"
  (let ((result (execute-lisp-capture-result program)))
    (prog1 'ignore
      (push-message "@server"
                    (format nil "user '~a' called lisp code '~a' ~a"
                            (client-name client) program result)))))
