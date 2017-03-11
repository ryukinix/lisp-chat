;; Common Lisp Script
;; Manoel Vilela

(ql:quickload :usocket)

(defpackage :lisp-chat-server
  (:use :usocket :cl)
  (:export :main))

(in-package :lisp-chat-server)

(load "./config.lisp") ;; *hosts* *port* *debug*

;; global vars
(defparameter *global-socket* (socket-listen *host* *port*))
(defparameter *clients* nil)
(defparameter *messages-stack* nil)
(defparameter *messages-log* nil)
(defparameter *commands-names* '("/users" "/help" "/log" "/quit"))

;; thread control
(defvar *message-semaphore* (sb-thread:make-semaphore :name "message semaphore" :count 0))
(defvar *message-mutex* (sb-thread:make-mutex :name "message mutex"))
(defvar *client-mutex* (sb-thread:make-mutex :name "client list mutex"))



(defstruct message from content time)

(defstruct client name socket)


(defun client-stream (c)
  (socket-stream (client-socket c)))


(defun debug-format (&rest args)
  (if *debug*
      (apply #'format args)))


(defun get-time ()
  (multiple-value-bind (second minute hour)
      (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))


(defun formated-message (message)
  (format nil "|~a| [~a]: ~a"
          (message-time message)
          (message-from message)
          (message-content message)))


(defun command-message (content)
  (let* ((from "@server")
         (time (get-time))
         (message (make-message :from from :content content :time time)))
    (formated-message message)))


;; user commands prefixed with /
(defun /users ()
  (command-message (format nil "~{~a~^, ~}" (mapcar #'client-name *clients*))))


(defun /help ()
  (command-message (format nil "~{~a~^, ~}" *commands-names*)))


(defun /log (&optional (depth 15))
  (format nil "~{~a~^~%~}" (reverse (subseq *messages-log* 0
                                            (min depth (length *messages-log*))))))


(defun push-message (from content)
  (sb-thread:with-mutex (*message-mutex*)
    (push (make-message :from from
                        :content content
                        :time (get-time))
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

(defun send-message (client message)
  (let ((stream (client-stream client)))
    (write-line message stream)
    (finish-output stream)))


(defun client-reader-routine (client)
  (loop for message = (read-line (client-stream client))
        while (not (equal message "/quit"))
        when (equal message "/users")
          do (send-message client (/users))
        when (equal message "/log")
          do (send-message client (/log))
        when (equal message "/help")
          do (send-message client (/help))
        when (and (> (length message) 0)
                  (not (member message *commands-names* :test #'equal)))
          do (push-message (client-name client)
                           message)
        finally (client-delete client)))


(defun client-reader (client)
  (handler-case (client-reader-routine client)
    (end-of-file () (client-delete client))))


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
      (sb-thread:make-thread #'client-reader
                             :name (format nil "~a reader thread" (client-name client))
                             :arguments (list client)))))


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
