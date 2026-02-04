(in-package :lisp-chat/server)

(defparameter *clients* nil "List of clients")
(defparameter *messages-stack* nil "Messages pending to be send by broadcasting")
(defparameter *messages-log* nil  "Messages log")
(defparameter *server-nickname* "@server" "The server nickname")


;; thread control
(defvar *message-semaphore* (make-semaphore :name "message semaphore"
                                            :count 0))
(defvar *client-lock* (make-lock "client list lock"))
(defvar *messages-lock* (make-lock "messages stack lock"))


(defstruct message
  "This structure abstract the type message with is saved
   into *messages-log* and until consumed, temporally pushed
   to *messages-stack*. FROM, CONTENT and TIME has type string"
  from
  content
  time )

(defstruct client
  "This structure handle the creation/control of the clients of the server.
   NAME is a string. Socket is a USOCKET:SOCKET and address is a ipv4 encoded
   string. "
  name
  socket
  address)

(defun get-client (client-name)
  (find client-name
        *clients*
        :test (lambda (name client) (equal name (client-name client)))))

(defun socket-peer-address (socket)
  "Given a USOCKET:SOCKET instance return a ipv4 encoded IP string"
  (format nil "~{~a~^.~}\:~a"
          (map 'list #'identity (get-peer-address socket))
          (get-peer-port socket)))

(defun client-stream (c)
  "Select the stream IO from the client"
  (socket-stream (client-socket c)))


(defun debug-format (&rest args)
  "If *debug* from lisp-chat-config is true, print debug info on
   running based on ARGS"
  (if *debug*
      (apply #'format args)))


(defun get-time ()
  "Return a encoded string as HH:MM:SS based on the current timestamp."
  (multiple-value-bind (second minute hour)
      (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))


(defun formatted-message (message)
  "The default message format of this server. MESSAGE is a struct message"
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

(defun private-message (client-name content)
  "This function prepare the CONTENT as a message by the @server"
  (let* ((from (format nil "dm:~a" client-name))
         (time (get-time))
         (message (make-message :from from
                                :content content
                                :time time)))
    (formatted-message message)))
