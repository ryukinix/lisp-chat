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
    (#.(system-interrupt) () (client-delete client))
    (error (c)
      (declare (ignore c))
      (client-delete client))))

(defun create-client (connection)
  "This procedure create a new client based on CONNECTION made by
  USOCKET:SOCKET-ACCEPT. This shit create a lot of side effects as messages
  if the debug is on because this makes all the log stuff to make analysis"
  (debug-format t "Incoming connection from ~a ~%" (socket-peer-address connection))
  (let ((client-stream (usocket:socket-stream connection)))
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
      (bt:with-lock-held (*client-lock*)
        (debug-format t "Added new user ~a@~a ~%"
                      (client-name client)
                      (client-address client))
        (push client *clients*))
      (user-joined-message client)
      (when history-channel
        (send-message client (command-message (format nil "You were restored to channel ~a" active-channel))))
      (bt:make-thread
         (lambda () (client-reader client))
         :name (format nil "reader-thread: ~a" (client-name client))))))

;; a function defined to handle the errors of client thread
(defun safe-client-thread (connection)
  "This function is a wrapper for CREATE-CLIENT treating the
exceptions."
  (handler-case (create-client connection)
    (end-of-file () nil)
    (usocket:address-in-use-error () nil)))

(defun interrupt-client-reader-threads ()
  (let ((prefix "reader-thread:"))
  (dolist (thread (bt:all-threads))
    (let ((name (bt:thread-name thread)))
      (when (and name
                 (> (length name) (length prefix))
                 (string= prefix (subseq name 0 (length prefix))))
        (when (bt:thread-alive-p thread)
          (interrupt-thread-portable thread)
          (bt:join-thread thread)))))))

(defun connection-handler (socket-server)
  "This is a special thread just for accepting connections from SOCKET-SERVER
   and creating new clients from it."
  (handler-case
       (loop for connection = (usocket:socket-accept socket-server)
         do (bt:make-thread (lambda () (safe-client-thread connection))
                         :name "create client"))
    (#.(system-interrupt) ()
      (interrupt-client-reader-threads))))

(defun socket-peer-address (socket)
  "Given a USOCKET:SOCKET instance return a ipv4 encoded IP string"
  (format nil "~{~a~^.~}\:~a"
          (map 'list #'identity (usocket:get-peer-address socket))
          (usocket:get-peer-port socket)))

(defun client-stream (c)
  "Select the stream IO from the client"
  (usocket:socket-stream (client-socket c)))
