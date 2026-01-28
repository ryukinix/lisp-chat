;; Common Lisp Script
;; Manoel Vilela

(defpackage #:lisp-chat/server
  (:use #:usocket
        #:cl
        #:lisp-chat/config
        #:bordeaux-threads)
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
  (:export #:main))

(in-package :lisp-chat/server)


;; global vars
(defvar *day-names* '("Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday" "Saturday" "Sunday")
  "Day names")
(defvar *uptime* (multiple-value-list (get-decoded-time))
  "Uptime of server variable")
(defparameter *commands-names*
  '("/users" "/help" "/log" "/quit" "/uptime" "/nick" "/ping")
  "Allowed command names to be called by client user")
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


(defun push-message (from content)
  "Push a messaged FROM as CONTENT into the *messages-stack*"
  (with-lock-held (*messages-lock*)
    (push (make-message :from from
                        :content content
                        :time (get-time))
          *messages-stack*))
  (signal-semaphore *message-semaphore*))

(defun client-close (client)
  (let ((socket (client-socket client)))
    (typecase socket
      (usocket:stream-usocket
       (socket-close socket))
      (t
       (websocket-driver:close-connection socket)))))

(defun client-delete (client)
  "Delete a CLIENT from the list *clients*"
  (let ((removed? nil))
    (with-lock-held (*client-lock*)
      (when (member client *clients*)
        (setf *clients* (remove client *clients*))
        (setf removed? t)))
    (when removed?
      (push-message "@server" (format nil "The user ~s exited from the party :("
                                      (client-name client)))
      (debug-format t "Deleted user ~a@~a~%"
                    (client-name client)
                    (client-address client))
      (client-close client))))

(defun send-message (client message)
  "Send to CLIENT a MESSAGE :type string"
  (let ((socket (client-socket client)))
    (typecase socket
      (usocket:stream-usocket
       (let ((stream (socket-stream socket)))
         (write-line message stream)
         (finish-output stream)))
      (t
       (websocket-driver:send socket message)))))

(defun startswith (string substring)
  "Check if STRING starts with SUBSTRING."
  (let ((l1 (length string))
        (l2 (length substring)))
    (when (and (> l2 0)
               (>= l1 l2))
      (loop for c1 across string
            for c2 across substring
            always (equal c1 c2)))))

(defun split (string delimiterp)
  "Split a string by a delimiterp function character checking"
  (loop for beg = (position-if-not delimiterp string)
          then (position-if-not delimiterp string :start (1+ end))
        for end = (and beg (position-if delimiterp string :start beg))
        when beg
          collect (subseq string beg end)
        while end))

(defun extract-params (string)
  (subseq (split string (lambda (c) (eql c #\Space)))
          1))

(defun call-command (client message)
  (let ((command (find message *commands-names* :test #'startswith)))
    (when command
      (call-command-by-name command (cons client
                                          (extract-params message))))))

(defun client-reader-routine (client)
  "This function create a IO-bound procedure to act
   by reading the events of a specific CLIENT.
   On this software each client talks on your own thread."
  (loop for message := (read-line (client-stream client))
        while (not (equal message "/quit"))
        for response := (call-command client message)
        if response
          do (send-message client response)
        else
          when (> (length message) 0)
            do (push-message (client-name client)
                             message)
        finally (client-delete client)))

(defun client-reader (client)
  "This procedure is a wrapper for CLIENT-READER-ROUTINE
   treating all the possible errors based on HANDLER-CASE macro."
  (handler-case (client-reader-routine client)
    (end-of-file () (client-delete client))
    (#+sbcl sb-int:simple-stream-error
     #-sbcl error
     ()
      (progn (debug-format t "~a@~a timed output"
                           (client-name client)
                           (client-address client))
             (client-delete client)))
    (#+sbcl sb-bsd-sockets:not-connected-error
     #-sbcl error
     ()
      (progn (debug-format t "~a@~a not connected more."
                           (client-name client)
                           (client-address client))
             (client-delete client)))))

(defun create-client (connection)
  "This procedure create a new client based on CONNECTION made by
  USOCKET:SOCKET-ACCEPT. This shit create a lot of side effects as messages
  if the debug is on because this makes all the log stuff to make analysis"
  (debug-format t "Incoming connection from ~a ~%" (socket-peer-address connection))
  (let ((client-stream (socket-stream connection)))
    (write-line "> Type your username: " client-stream)
    (finish-output client-stream)
    (let ((client (make-client :name (read-line client-stream)
                               :socket connection
                               :address (socket-peer-address connection))))
      (with-lock-held (*client-lock*)
        (debug-format t "Added new user ~a@~a ~%"
                      (client-name client)
                      (client-address client))
        (push client *clients*))
      (push-message "@server" (format nil "The user ~s joined to the party!" (client-name client)))
      (make-thread (lambda () (client-reader client))
                   :name (format nil "~a reader thread" (client-name client))))))

;; a function defined to handle the errors of client thread
(defun safe-client-thread (connection)
  "This function is a wrapper for CREATE-CLIENT treating the
exceptions."
  (handler-case (create-client connection)
    (end-of-file () nil)
    (usocket:address-in-use-error () nil)))

(defun message-broadcast ()
  "This procedure is a general independent thread to run brodcasting
   all the clients when a message is ping on this server"
  (loop when (wait-on-semaphore *message-semaphore*)
          do (let* ((message-raw (with-lock-held (*messages-lock*)
                                   (pop *messages-stack*)))
                    (message (formatted-message message-raw)))
               (push message-raw *messages-log*)
               (let ((clients (with-lock-held (*client-lock*) (copy-list *clients*))))
                 (loop for client in clients
                       do (handler-case (send-message client message)
                            (error (e)
                              (debug-format t "Error broadcasting to ~a: ~a~%" (client-name client) e)
                              (client-delete client))))))))

(defun ws-app (env)
  (let ((ws (make-server env))
        (client nil))
    (on :message ws
        (lambda (message)
          ;; (debug-format t "Received WS message: ~s~%" message)
          (if (null client)
              (let ((name (string-trim '(#\Space #\Return #\Newline) message)))
                (if (zerop (length name))
                    (send ws "> Name cannot be empty. Try again: ")
                    (progn
                      (setf client (make-client :name name
                                                :socket ws
                                                :address (getf env :remote-addr)))
                      (with-lock-held (*client-lock*)
                        (push client *clients*))
                      (push-message "@server" (format nil "The user ~s joined to the party!" name))
                      (debug-format t "New web-socket user ~a@~a~%" name (client-address client)))))
              (let ((response (call-command client message)))
                (if response
                    (send-message client response)
                    (when (> (length message) 0)
                      (push-message (client-name client) message)))))))
    (on :open ws
        (lambda ()
          (send ws "> Type your username: ")))
    (on :close ws
        (lambda (&key code reason)
          (declare (ignore code reason))
          (when client
            (client-delete client))))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defun static-app (path)
  (let* ((file (merge-pathnames (subseq path 1) (merge-pathnames "src/static/"
                                                                (asdf:system-source-directory :lisp-chat))))
         (extension (pathname-type file))
         (content-type (cond
                         ((string= extension "html") "text/html")
                         ((string= extension "css") "text/css")
                         ((string= extension "js") "application/javascript")
                         (t "text/plain"))))
    (if (probe-file file)
        `(200 (:content-type ,content-type) ,file)
        '(404 (:content-type "text/plain") ("Not Found")))))

(defun web-handler (env)
  (let ((path (getf env :path-info)))
    (cond
      ((string= path "/ws")
       (ws-app env))
      ((string= path "/")
       (static-app "/index.html"))
      (t
       (static-app path)))))

(defun connection-handler (socket-server)
  "This is a special thread just for accepting connections from SOCKET-SERVER
   and creating new clients from it."
  (loop for connection = (socket-accept socket-server)
        do (make-thread (lambda () (safe-client-thread connection))
                        :name "create client")))

(defun server-loop (socket-server)
  "This is the general server-loop procedure. Create the threads
   necessary for the basic working state of this chat. The main idea
   is creating a MESSAGE-BROADCAST procedure and CONNECTION-HANDLER
   procedure running as separated threads.

   The first procedure send always a new message too all clients
   defined on *clients* when *messages-semaphore* is signalized.
   The second procedure is a general connection-handler for new
   clients trying connecting to the server."
  (format t "Running server at ~a:~a... ~%" *host* *port*)
  (format t "Running web server at http://~a:~a... ~%" *host* *websocket-port*)
  (let (connection-thread
        broadcast-thread
        web-handler)
    (unwind-protect
         (progn
           (setf connection-thread (make-thread (lambda () (connection-handler socket-server))
                                                :name "Connection handler"))
           (setf broadcast-thread (make-thread #'message-broadcast
                                               :name "Message broadcast"))
           (setf web-handler (clackup #'web-handler
                                      :address *host*
                                      :port *websocket-port*
                                      :use-thread t))
           (join-thread connection-thread)
           (join-thread broadcast-thread))
      (progn
        (debug-format t "~%Shutting down...~%")
        (when web-handler
          (debug-format t "Stopping web server...~%")
          (handler-case (stop web-handler)
            (error (c) (debug-format t "Error stopping web server: ~a~%" c))))
        (when (and connection-thread (thread-alive-p connection-thread))
          (debug-format t "Stopping connection handler...~%")
          (destroy-thread connection-thread))
        (when (and broadcast-thread (thread-alive-p broadcast-thread))
          (debug-format t "Stopping message broadcast...~%")
          (destroy-thread broadcast-thread))
        (let ((clients (with-lock-held (*client-lock*) (copy-list *clients*))))
          (loop for client in clients
                do (client-close client)))))))

(defun main (&key (host *host*) (port *port*))
  "Well, this function run all the necessary shits."
  (let ((socket-server nil)
        (error-code 0))
    (unwind-protect
         (handler-case
             (progn (setq socket-server (socket-listen host port))
                    (server-loop socket-server))
           (usocket:address-in-use-error ()
             (format *error-output*
                     "error: Address:port at ~a\:~a already busy.~%"
                     *host*
                     *port*)
             (setq error-code 1))
           (usocket:address-not-available-error ()
             (format *error-output*
                     "error: There is no way to use ~a as host to run the server.~%"
                     *host*)
             (setq error-code 2))
           (#+sbcl sb-sys:interactive-interrupt
            #+ccl  ccl:interrupt-signal-condition
            #+clisp system::simple-interrupt-condition
            #+ecl ext:interactive-interrupt
            #+allegro excl:interrupt-signal ()
             (format t "~%Closing the server...~%")))
      (when socket-server
        (socket-close socket-server))
      (uiop:quit error-code))))
