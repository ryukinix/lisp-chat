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
  "Return a list separated by commas of the currently logged users"
  (declare (ignorable client args))
  (command-message (format nil "users: ~{~a~^, ~}" (mapcar #'client-name *clients*))))

(defun /ping (client &rest args)
  "Return a list separated by commas of the currently logged users"
  (declare (ignorable client args))
  (command-message (format nil "pong ~a" (or args (client-name client)))))


(defun /help (client &rest args)
  "Show a list of the available commands of lisp-chat"
  (declare (ignorable client args))
  (command-message (format nil "~{~a~^, ~}" (get-commands))))

(defun /log (client &optional (depth "20") &rest args)
  "Show the last messages typed on the server.
   DEPTH is optional number of messages frames from log"
  (declare (ignorable client args))
  (let* ((messages (user-messages))
         (log-size (min (or (parse-integer depth :junk-allowed t) 20)
                        (length messages))))
    (format nil "~{~a~^~%~}" (reverse (subseq messages 0
                                              log-size)))))

(defun /uptime (client &rest args)
  "Return a string nice encoded to preset the uptime since the server started."
  (declare (ignorable client args))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (values-list *uptime*)
    (declare (ignore dst-p))
    (command-message
     (format nil
             "Server online since ~2,'0d:~2,'0d:~2,'0d of ~a, ~2,'0d/~2,'0d/~d (GMT~@d)"
             hour minute second
             (nth day-of-week *day-names*)
             month date year
             (- tz)))))

(defun /nick (client &optional (new-nick nil) &rest args)

  "Change the client-name given a NEW-NICK which should be a string"

  (declare (ignorable args))

  (if new-nick

      (progn (setf (client-name client) new-nick)

             (command-message (format nil "Your new nick is: ~a" new-nick)))

      (command-message (format nil "/nick <new-nickname>"))))



(defun /quit (client &rest args)

  "Quit the chat"

  (declare (ignore client args))

  (command-message "Bye!"))
