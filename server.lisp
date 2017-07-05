;; Common Lisp Script
;; Manoel Vilela

(when (not (find-package 'usocket))
  (ql:quickload :usocket))

(when (not (find-package 'lisp-chat-config))
  (load "config"))


(defpackage :lisp-chat-server
  (:use :usocket :cl :lisp-chat-config)
  (:export :main))

(in-package :lisp-chat-server)

;; time related
(defparameter *uptime* (multiple-value-list (get-decoded-time)))
(defparameter *day-names*
  '("Monday" "Tuesday" "Wednesday"
    "Thursday" "Friday" "Saturday"
    "Sunday"))

;; global vars
(defparameter *clients* nil)
(defparameter *messages-stack* nil)
(defparameter *messages-log* nil)
(defparameter *commands-names* '("/users" "/help" "/log" "/quit" "/uptime"))


;; thread control
(defvar *message-semaphore* (sb-thread:make-semaphore :name "message semaphore" :count 0))
(defvar *client-mutex* (sb-thread:make-mutex :name "client list mutex"))


(defstruct message from content time)

(defstruct client name socket address)


(defun socket-peer-address (socket)
  (format nil "~{~a~^.~}\:~a"
          (map 'list #'identity (get-peer-address socket))
          (get-peer-port socket)))

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

(defun call-command-by-name (string)
  (funcall (find-symbol (string-upcase string) :lisp-chat-server)))

;; user commands prefixed with /
(defun /users ()
  (command-message (format nil "~{~a~^, ~}" (mapcar #'client-name *clients*))))


(defun /help ()
  (command-message (format nil "~{~a~^, ~}" *commands-names*)))


(defun /log (&optional (depth 15))
  (format nil "~{~a~^~%~}" (reverse (subseq *messages-log* 0
                                            (min depth (length *messages-log*))))))

(defun /uptime ()
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (values-list *uptime*)
    (declare (ignore dst-p))
    (command-message (format nil "Server online since ~2,'0d:~2,'0d:~2,'0d of ~a, ~2,'0d/~2,'0d/~d (GMT~@d)"
                             hour
                             minute
                             second
                             (nth day-of-week *day-names*)
                             month
                             date
                             year
                             (- tz)))))


(defun push-message (from content)
  (push (make-message :from from
                      :content content
                      :time (get-time))
        *messages-stack*)
  (sb-thread:signal-semaphore *message-semaphore*))

(defun client-delete (client)
  (sb-thread:with-mutex (*client-mutex*)
    (setf *clients* (remove-if (lambda (c)
                                 (equal (client-address c)
                                        (client-address client)))
                               *clients*)))
  (push-message "@server" (format nil "The user ~s exited from the party :("
                                  (client-name client)))
  (debug-format t "Deleted user ~a@~a~%"
                (client-name client)
                (client-address client))
  (socket-close (client-socket client)))

(defun send-message (client message)
  (let ((stream (client-stream client)))
    (write-line message stream)
    (finish-output stream)))

(defun client-reader-routine (client)
  (loop for message = (read-line (client-stream client))
        while (not (equal message "/quit"))
        when (member message *commands-names* :test #'equal)
          do (send-message client (call-command-by-name message))
        when (and (> (length message) 0)
                  (not (member message *commands-names* :test #'equal)))
          do (push-message (client-name client)
                           message)
        finally (client-delete client)))

(defun client-reader (client)
  (handler-case (client-reader-routine client)
    (end-of-file () (client-delete client))
    (sb-int:simple-stream-error () (progn (debug-format t "~a@~a timed output"
                                                        (client-name client)
                                                        (client-address client))
                                          (client-delete client)))
    (sb-bsd-sockets:not-connected-error () (progn (debug-format t "~a@~a not connected more."
                                                                (client-name client)
                                                                (client-address client))
                                                  (client-delete client)))))

(defun create-client (connection)
  (debug-format t "Incoming connection from ~a ~%" (socket-peer-address connection))
  (let ((client-stream (socket-stream connection)))
    (write-line "> Type your username: " client-stream)
    (finish-output client-stream)
    (let ((client (make-client :name (read-line client-stream)
                               :socket connection
                               :address (socket-peer-address connection))))
      (sb-thread:with-mutex (*client-mutex*)
        (debug-format t "Added new user ~a@~a ~%"
                      (client-name client)
                      (client-address client))
        (push client *clients*))
      (push-message "@server" (format nil "The user ~s joined to the party!" (client-name client)))
      (sb-thread:make-thread #'client-reader
                             :name (format nil "~a reader thread" (client-name client))
                             :arguments (list client)))))

(defun message-broadcast ()
  (loop when (sb-thread:wait-on-semaphore *message-semaphore*)
          do (let ((message (formated-message (pop *messages-stack*))))
               (push message *messages-log*)
               (loop for client in *clients*
                     do (handler-case (send-message client message)
                          (sb-int:simple-stream-error () (client-delete client))
                          (sb-bsd-sockets:not-connected-error () (client-delete client)))))))

(defun connection-handler (socket-server)
  (loop for connection = (socket-accept socket-server)
        do (sb-thread:make-thread #'create-client
                                  :arguments (list connection)
                                  :name "create client")))

(defun server-loop (socket-server)
  (format t "Running server... ~%")
  (let* ((connection-thread (sb-thread:make-thread #'connection-handler
                                                   :arguments (list socket-server)
                                                   :name "Connection handler"))
         (broadcast-thread (sb-thread:make-thread #'message-broadcast
                                                  :name "Message broadcast")))
    (sb-thread:join-thread connection-thread)
    (sb-thread:join-thread broadcast-thread)))

(defun main ()
  (let ((socket-server (socket-listen *host* *port*)))
    (unwind-protect (handler-case (server-loop socket-server)
                      (usocket:address-in-use-error () (format t "Address ~a\@~a already busy."
                                                               *host*
                                                               *port*))
                      (sb-sys:interactive-interrupt () (format t "Closing the server...")))
      (socket-close socket-server)))
  (sb-ext:exit))
