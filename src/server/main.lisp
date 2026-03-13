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
  (format t "Running tcp server at ~a:~a... ~%" *host* *port*)
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
           (setf web-handler (clackup *app*
                                      :debug nil
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
            (condition (c) (debug-format t "Error stopping web server: ~a~%" c))))
        (when (and connection-thread (thread-alive-p connection-thread))
          (debug-format t "Stopping connection handler...~%")
          (interrupt-thread-portable connection-thread)
          (join-thread connection-thread))
        ;; delete residual web clients...
        (dolist (client *clients*)
          (client-delete client))
        (when (and broadcast-thread (thread-alive-p broadcast-thread))
          (debug-format t "Stopping message broadcast...~%")
          (destroy-thread broadcast-thread))))))

(defun main (&key (host *host*) (port *port*) (should-quit t))
  "Well, this function run all the necessary shits."
  (load-persistent-messages)
  (let ((socket-server nil)
        (error-code 0))
    (unwind-protect
         (handler-case
             (progn (setq socket-server (socket-listen host port :reuse-address t))
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
           (#.(system-interrupt) ()
             (format t "~%Closing the lisp-chat server...~%")))
      (when socket-server
        (socket-close socket-server))
      (when should-quit
        (uiop:quit error-code)))))
