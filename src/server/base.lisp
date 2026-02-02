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

(defun startswith (string substring)
  "Check if STRING starts with SUBSTRING."
  (let ((l1 (length string))
        (l2 (length substring)))
    (when (and (> l2 0)
               (>= l1 l2))
      (loop for c1 across string
            for c2 across substring
            always (equal c1 c2)))))

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

(defun split (string delimiterp)
  "Split a string by a delimiterp function character checking"
  (loop for beg = (position-if-not delimiterp string)
          then (position-if-not delimiterp string :start (1+ end))
        for end = (and beg (position-if delimiterp string :start beg))
        when beg
          collect (subseq string beg end)
        while end))
