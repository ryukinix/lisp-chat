;; Common Lisp Script
;; Manoel Vilela

(ql:quickload :usocket)

(defpackage :lisp-chat-server
  (:use :usocket :cl)
  (:export :main))

(in-package :lisp-chat-server)

(load "./config.lisp") ;; *hosts* *port* *debug*

(defparameter *global-socket* (socket-listen *host* *port*))

(defparameter *clients* nil)
(defparameter *client-read-threads* nil)
(defparameter *messages-stack* nil)
(defparameter *messages-log* nil)

(defvar *message-semaphore* (sb-thread:make-semaphore :name "message semaphore" :count 0))
(defvar *message-mutex* (sb-thread:make-mutex :name "message mutex"))
(defvar *client-mutex* (sb-thread:make-mutex :name "client list mutex"))
(defvar *client-read-mutex* (sb-thread:make-mutex :name "client read mutex"))


(defstruct client name socket)

(defstruct message from content timestamp)


(defun debug-format (&rest args)
  (if *debug*
      (apply #'format args)))


(defun client-stream (c)
  (socket-stream (client-socket c)))


(defun formated-message (message)
  (format nil "[~a]: ~a"
          (message-from message)
          (message-content message)))


(defun push-message (from message)
  (sb-thread:with-mutex (*message-mutex*)
    (push (make-message :from from
                        :content message
                        :timestamp "NOT IMPLEMENTED" )
          *messages-stack*)
    (sb-thread:signal-semaphore *message-semaphore*)))


(defun client-delete (client)
  (push-message "@server"
                (format nil "The user ~s exited from the party :(" (client-name client)))
  (socket-close (client-socket client))
  (sb-thread:with-mutex (*client-mutex*)
    (setf *clients* (remove-if (lambda (c)
                                 (equal (client-name c)
                                        (client-name client)))
                               *clients*)))
  (debug-format t "Deleted user ~s ~%" (client-name client)))


(defun client-reader-routine (client)
  (loop for message = (read-line (client-stream client))
        while (not (equal message "/quit"))
        when (> (length message) 0)
          do (push-message (client-name client)
                           message)
        finally (client-delete client)))

(defun client-reader (client)
  (handler-case (client-reader-routine client)
    (end-of-file () (client-delete client))))


(defun send-message (client message)
  (let ((stream (client-stream client)))
    (write-line message stream)
    (finish-output stream)))


(defun create-client (connection)
  (debug-format t "Incoming connection from ~{~a~^.~}\:~a ~%"
                (map 'list #'identity (get-peer-address connection))
                (get-peer-port connection))
  (let ((client-stream (socket-stream connection)))
    (write-line "> Type your username: " client-stream)
    (finish-output client-stream)
    (let ((client (make-client :name (read-line client-stream)
                              :socket connection)))
      (sb-thread:with-mutex (*client-mutex*)
        (debug-format t "Added new user ~s ~%" (client-name client))
        (push client *clients*))
      (push-message "@server" (format nil "The user ~s joined to the party!" (client-name client)))
      (sb-thread:with-mutex (*client-read-mutex*)
        (push (sb-thread:make-thread #'client-reader
                                     :name (format nil "~a reader thread" (client-name client))
                                     :arguments (list client))
              *client-read-threads*)))))


(defun message-broadcast ()
  (loop when (sb-thread:wait-on-semaphore *message-semaphore*)
          do (sb-thread:with-mutex (*message-mutex*)
               (let ((message (formated-message (pop *messages-stack*))))
                 (push message *messages-log*)
                 (loop for client in *clients*
                    do (handler-case (send-message client message)
                         (sb-int:simple-stream-error () (client-delete client))))))))

(defun connection-handler ()
  (loop for connection = (socket-accept *global-socket*)
        do (sb-thread:make-thread #'create-client
                                  :arguments (list connection)
                                  :name "create client")))


(defun server-loop ()
  (format t "Running server... ~%")
  (let* ((connection-thread (sb-thread:make-thread #'connection-handler
                                                   :name "Connection handler"))
         (broadcast-thread (sb-thread:make-thread #'message-broadcast
                                                  :name "Message broadcast")))
    (sb-thread:join-thread connection-thread)
    (sb-thread:join-thread broadcast-thread)))


(defun main ()
  (handler-case (server-loop)
    (usocket:address-in-use-error () (format t "Address ~a\@~a already busy."
                                             *host*
                                             *port*))
    (sb-sys:interactive-interrupt () (format t "Closing the server...")))

  (socket-close *global-socket*)
  (sb-ext:exit))

(main)
