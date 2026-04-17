(in-package :lisp-chat/server)
(defparameter *clients* nil "List of clients")
(defparameter *messages-stack* nil "Messages pending to be send by broadcasting")
(defparameter *messages-log* nil  "Messages log")
(defparameter *user-channels* (make-hash-table :test 'equal) "Mapping of usernames to their last active channel")
(defparameter *private-channels* (make-hash-table :test 'equal) "Set of channels where messages are not saved")

(defvar *server-nickname* "@server" "The server nickname")
(defvar *raw-command-message* nil "If true, return raw strings instead of formatted-messages")

;; thread control
(defvar *message-semaphore* (bt:make-semaphore :name "message semaphore"
                                            :count 0))
(defvar *client-lock* (bt:make-lock "client list lock"))
(defvar *messages-lock* (bt:make-lock "messages stack lock"))
(defvar *day-names* '("Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday" "Saturday" "Sunday")
  "Day names")
(defvar *uptime* nil "Uptime of server variable, initialized at server start")

(defun system-interrupt ()
  #+sbcl 'sb-sys:interactive-interrupt
  #+ccl  'ccl:interrupt-signal-condition
  #+clisp 'system::simple-interrupt-condition
  #+ecl 'ext:interactive-interrupt
  #+allegro 'excl:interrupt-signal)

(defun interrupt-thread-portable (thread)
  (bt:interrupt-thread thread
                       (lambda () (error (system-interrupt)))))

(defun client-socket-type (client)
  "Return the socket type for the given client."
  (typecase (client-socket client)
    (usocket:stream-usocket "TCP")
    (t "WebSocket")))

(defun get-client (client-name)
  "Get client by name"
  (find client-name
        *clients*
        :test (lambda (name client) (equal name (client-name client)))))

(defun get-client-by-session (session-id)
  (find session-id
        *clients*
        :test (lambda (sid client) (string-equal sid (client-session-id client)))))

(defun socket-peer-address (socket)
  "Given a USOCKET:SOCKET instance return a ipv4 encoded IP string"
  (format nil "~{~a~^.~}\:~a"
          (map 'list #'identity (usocket:get-peer-address socket))
          (usocket:get-peer-port socket)))

(defun client-stream (c)
  "Select the stream IO from the client"
  (usocket:socket-stream (client-socket c)))

(defun debug-format (&rest args)
  "If config:*debug* from lisp-chat-config is true, print debug info on
   running based on ARGS"
  (when config:*debug*
      (apply #'format args)))

(defun get-time ()
  "Return a encoded string as HH:MM:SS based on the current timestamp."
  (multiple-value-list (get-decoded-time)))

(defun command-message (content)
  "This function prepare the CONTENT as a message by the @server"
  (if *raw-command-message*
      content
      (let* ((from *server-nickname*)
             (time (get-time))
             (message (make-message :from from :content content :time time)))
        (formatted-message message))))

(defun private-message (client-name content)
  "This function prepare the CONTENT as a message by the @server"
  (let* ((from (format nil "dm:~a" client-name))
         (time (get-time))
         (message (make-message :from from
                                :content content
                                :time time)))
    (formatted-message message)))

(defun reset-server ()
  "Reset the server state."
  (bt:with-lock-held (*client-lock*)
    (setf *clients* nil))
  (bt:with-lock-held (*messages-lock*)
    (setf *messages-stack* nil))
  (setf *user-channels* (make-hash-table :test 'equal))
  (setf *private-channels* (make-hash-table :test 'equal))
  (setf *messages-log* nil))
