(in-package :lisp-chat/server)

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
