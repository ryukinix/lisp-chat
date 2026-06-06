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
  (format t "Running tcp server at ~a:~a... ~%" config:*host* config:*port*)
  (format t "Running web server at http://~a:~a... ~%" config:*host* config:*websocket-port*)
  (let (connection-thread
        broadcast-thread
        web-handler)
    (unwind-protect
         (progn
           (setf connection-thread (bt:make-thread (lambda () (connection-handler socket-server))
                                                :name "Connection handler"))
           (setf broadcast-thread (bt:make-thread #'message-broadcast
                                               :name "Message broadcast"))
           (bt:make-thread #'persistence-worker :name "Persistence worker")
           (setf web-handler (clackup *app*
                                      :debug nil
                                      :address config:*host*
                                      :port config:*websocket-port*
                                      :use-thread t))
           (bt:join-thread connection-thread)
           (bt:join-thread broadcast-thread))
      (progn
        (debug-format t "~%Shutting down...~%")
        (when web-handler
          (debug-format t "Stopping web server...~%")
          (handler-case (stop web-handler)
            (condition (c) (debug-format t "Error stopping web server: ~a~%" c))))
        (when (and connection-thread (bt:thread-alive-p connection-thread))
          (debug-format t "Stopping connection handler...~%")
          (interrupt-thread-portable connection-thread)
          (bt:join-thread connection-thread))
        ;; delete residual web clients...
        (dolist (client *clients*)
          (client-delete client))
        (when (and broadcast-thread (bt:thread-alive-p broadcast-thread))
          (debug-format t "Stopping message broadcast...~%")
          (bt:destroy-thread broadcast-thread))))))

(defun main (&key (host config:*host*) (port config:*port*) (should-quit t))
  "Main entrypoint to start the tcp-server and http server (with websockets)"
  (setq *uptime* (get-time))
  (let ((pub (uiop:getenv "VAPID_PUBLIC_KEY"))
        (priv (uiop:getenv "VAPID_PRIVATE_KEY"))
        (vapid-file "vapid.sexp"))
    (cond
      ((and pub priv)
       (setq *vapid-public-key* pub)
       (setq *vapid-private-key* priv))
      ((probe-file vapid-file)
       (with-open-file (in vapid-file :direction :input)
         (let ((keys (read in nil nil)))
           (setq *vapid-public-key* (first keys))
           (setq *vapid-private-key* (second keys)))))
      (t
       (multiple-value-bind (new-pub new-priv) (cl-web-push:generate-vapid-keys)
         (setq *vapid-public-key* new-pub)
         (setq *vapid-private-key* new-priv)
         (with-open-file (out vapid-file :direction :output :if-exists :supersede :if-does-not-exist :create)
           (print (list new-pub new-priv) out))))))
  (load-persistent-messages)
  (load-push-subscriptions)
  (let ((socket-server nil)
        (error-code 0))
    (unwind-protect
         (handler-case
             (progn (setq socket-server (usocket:socket-listen host port :reuse-address t))
                    (server-loop socket-server))
           (usocket:address-in-use-error ()
             (format *error-output*
                     "error: Address:port at ~a\:~a already busy.~%"
                     config:*host*
                     config:*port*)
             (setq error-code 1))
           (usocket:address-not-available-error ()
             (format *error-output*
                     "error: There is no way to use ~a as host to run the server.~%"
                     config:*host*)
             (setq error-code 2))
           (#.(system-interrupt) ()
             (format t "~%Closing the lisp-chat server...~%")))
      (when socket-server
        (usocket:socket-close socket-server))
      (when should-quit
        (uiop:quit error-code)))))
