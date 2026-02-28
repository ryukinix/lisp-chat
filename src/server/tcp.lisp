(in-package :lisp-chat/server)

(defun client-reader-routine (client)
  "This function create a IO-bound procedure to act
   by reading the events of a specific CLIENT.
   On this software each client talks on your own thread."
  (loop for message := (read-line (client-stream client))
        while (not (equal message "/quit"))
        for response := (lisp-chat/commands:call-command client message)
        if response
          do (send-message client response)
        else
          when (> (length message) 0)
            do (push-message (client-name client)
                             message :channel (client-active-channel client))
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
    (let* ((name (read-line client-stream))
           (history-channel (gethash name *user-channels*))
           (active-channel (or history-channel "#general"))
           (client (make-client :name name
                                :socket connection
                                :address (socket-peer-address connection)
                                :time (get-time)
                                :active-channel active-channel)))
      (setf (gethash name *user-channels*) active-channel)
      (with-lock-held (*client-lock*)
        (debug-format t "Added new user ~a@~a ~%"
                      (client-name client)
                      (client-address client))
        (push client *clients*))
      (user-joined-message client)
      (when history-channel
        (send-message client (command-message (format nil "You were restored to channel ~a" active-channel))))
      (make-thread (lambda () (client-reader client))
                   :name (format nil "~a reader thread" (client-name client))))))

;; a function defined to handle the errors of client thread
(defun safe-client-thread (connection)
  "This function is a wrapper for CREATE-CLIENT treating the
exceptions."
  (handler-case (create-client connection)
    (end-of-file () nil)
    (usocket:address-in-use-error () nil)))

(defun connection-handler (socket-server)
  "This is a special thread just for accepting connections from SOCKET-SERVER
   and creating new clients from it."
  (loop for connection = (socket-accept socket-server)
        do (make-thread (lambda () (safe-client-thread connection))
                        :name "create client")))
