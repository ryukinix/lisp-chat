(in-package :lisp-chat/server)

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

(defun message-broadcast ()
  "This procedure is a general independent thread to run brodcasting
   all the clients when a message is ping on this server"
  (loop when (wait-on-semaphore *message-semaphore*)
          do (let* ((message-raw (with-lock-held (*messages-lock*)
                                   (pop *messages-stack*)))
                    (message (formatted-message message-raw)))
               (push message-raw *messages-log*)
               (let ((clients *clients*))
                 (loop for client in clients
                       do (handler-case (send-message client message)
                            (error (e)
                              (debug-format t "Error broadcasting to ~a: ~a~%" (client-name client) e)
                              (client-delete client))))))))
