(in-package :lisp-chat/server)

(defparameter *commands-names*
  '("/users" "/help" "/log" "/quit" "/uptime" "/nick" "/ping")
  "Allowed command names to be called by client user")

(defun formatted-message (message)
  "The default message format of this server. MESSAGE is a string
   Changing this reflects all the layout from client/server.
   Probably this would be the MFRP: Manoel Fucking Raw Protocol.
   Because this we can still use netcat as client for lisp-chat."
  (format nil "|~a| [~a]: ~a"
          (message-time message)
          (message-from message)
          (message-content message)))

(defun user-messages ()
  "Return only user messages, discard all messsages from @server"
  (mapcar #'formatted-message
          (remove-if #'(lambda (m) (equal (message-from m) "@server"))
                     *messages-log*)))

(defun command-message (content)
  "This function prepare the CONTENT as a message by the @server"
  (let* ((from *server-nickname*)
         (time (get-time))
         (message (make-message :from from :content content :time time)))
    (formatted-message message)))

(defun call-command-by-name (string params)
  "Wow, this is a horrible hack to get a string as symbol for functions/command
  like /help /users /log and so on."
  (let ((command-function (find-symbol (string-upcase string) :lisp-chat/server)))
    (when command-function
      (apply command-function params))))

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
  (command-message (format nil "~{~a~^, ~}" *commands-names*)))

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

(defun extract-params (string)
  (subseq (split string (lambda (c) (eql c #\Space)))
          1))

(defun call-command (client message)
  (let ((command (find message *commands-names* :test #'startswith)))
    (when command
      (call-command-by-name command (cons client
                                          (extract-params message))))))
