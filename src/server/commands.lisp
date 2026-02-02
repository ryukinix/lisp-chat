(in-package :lisp-chat/commands)

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

(defun extract-params (string)
  (subseq (split string (lambda (c) (eql c #\Space)))
          1))

(defun call-command (client message)
  (let ((command (find message (get-commands) :test #'startswith)))
    (when command
      (let ((command-function (get-command command)))
        (when command-function
          (apply command-function (cons client (extract-params message))))))))

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

(defun /log (client &optional (depth "20") &rest args)
  "/log shows the last messages sent to the server.
   DEPTH is optional number of messages frames from log"
  (declare (ignorable client args))
  (let* ((messages (user-messages))
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
             "Server online since ~2,'0d:~2,'0d:~2,'0d of ~a, ~2,'0d-~2,'0d-~d (GMT~@d)"
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
