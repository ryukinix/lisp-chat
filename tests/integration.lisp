(defpackage :lisp-chat/tests
  (:use :cl :parachute)
  (:import-from :lisp-chat/config :*port* :*websocket-port* :*host* :*debug*)
  (:import-from :usocket :socket-connect :socket-close :socket-stream)
  (:import-from :websocket-driver :start-connection :send :on :close-connection)
  (:import-from :websocket-driver-client :make-client)
  (:export :run-tests))

(in-package :lisp-chat/tests)

(define-test lisp-chat-tests
  :description "Integration tests for lisp-chat")

(defvar *server-thread* nil)
(defparameter *print-names* t)
(defparameter *port* 9998)
(defparameter *debug* nil)
(defparameter *websocket-port* 9999)

(defun get-current-date ()
    (multiple-value-bind
          (second minute hour date month year)
        (get-decoded-time)
      (declare (ignore second minute hour))
      (format nil "~4d-~2,'0d-~2,'0d"
              year month date)))

(defun start-test-server ()
  (setf *server-thread*
        (bt:make-thread (lambda ()
                          (let ((*standard-output* (make-broadcast-stream))
                                (*error-output* (make-broadcast-stream)))
                            (lisp-chat/server:main :should-quit nil)))
                        :name "Test Server"))
  (sleep 2))

(defun stop-test-server ()
  (when (and *server-thread* (bt:thread-alive-p *server-thread*))
    #-sbcl (bt:destroy-thread *server-thread*)
    #+sbcl (bt:interrupt-thread *server-thread* (lambda () (error 'sb-sys:interactive-interrupt))))
  (setf *server-thread* nil)
  (sleep 1))

(defun run-tests ()
  (start-test-server)
  (unwind-protect
       (parachute:test 'lisp-chat-tests)
    (stop-test-server)))

(define-test tcp-client-connection
  :parent lisp-chat-tests
  (let ((socket (usocket:socket-connect "127.0.0.1" *port*))
        (stream nil))
    (setf stream (usocket:socket-stream socket))
    (true socket)
    ;; Read prompt
    (let ((prompt (read-line stream)))
      (true (search "> Type your username: " prompt)))
    ;; Send username
    (write-line "tester-tcp" stream)
    (finish-output stream)
    ;; Read welcome
    (let ((welcome (read-line stream)))
      (true (search "\"tester-tcp\" joined to the party" welcome)))
    (usocket:socket-close socket)))

(define-test websocket-client-connection
  :parent lisp-chat-tests
  (let* ((url (format nil "ws://127.0.0.1:~a/ws" *websocket-port*))
         (client (make-client url))
         (connected nil)
         (messages '()))

    (on :open client (lambda () (setf connected t)))
    (on :message client (lambda (msg) (push msg messages)))

    (start-connection client)
    (sleep 1)
    (true connected)

    ;; Send username (server asks for it first? server sends "> Type your username: ")
    ;; Wait a bit for prompt
    (sleep 0.5)
    (true (some (lambda (m) (search "Type your username" m)) messages))

    (send client "tester-ws")
    (sleep 0.5)

    (true (some (lambda (m) (search "\"tester-ws\" joined to the party" m)) messages))

    (close-connection client)))

(define-test log-commands-with-date-format
  :parent lisp-chat-tests
  (let ((socket (usocket:socket-connect "127.0.0.1" *port*))
        (stream nil))
    (setf stream (usocket:socket-stream socket))
    (true socket)
    ;; Read prompt
    (let ((prompt (read-line stream)))
      (true (search "> Type your username: " prompt)))
    ;; Send username
    (write-line "tester-log" stream)
    (finish-output stream)
    (read-line stream) ;; ignore welcome message
    ;; Send a message to ensure something is in the log
    (write-line "hello log" stream)
    (finish-output stream)
    (sleep 0.5) ;; wait for broadcast to log
    (read-line stream) ;; consume the broadcast of "hello log"
    (write-line "/log :date-format date" stream)
    (finish-output stream)
    (let ((msg (read-line stream)))
      (when *debug* (format t "Log output: [~a]~%" msg))
      (true (search (get-current-date) msg)))
    (usocket:socket-close socket)))
