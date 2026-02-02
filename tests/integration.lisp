(defpackage :lisp-chat/tests
  (:use :cl :fiveam)
  (:import-from :lisp-chat/config :*port* :*websocket-port* :*host*)
  (:import-from :usocket :socket-connect :socket-close :socket-stream)
  (:import-from :websocket-driver :start-connection :send :on :close-connection)
  (:import-from :websocket-driver-client :make-client)
  (:export :run-tests))

(in-package :lisp-chat/tests)

(def-suite :lisp-chat-tests
  :description "Integration tests for lisp-chat")

(in-suite :lisp-chat-tests)

(defvar *server-thread* nil)
(defparameter *port* 9998)
(defparameter *websocket-port* 9999)

(defun start-test-server ()
  (setf *server-thread*
        (bt:make-thread (lambda ()
                          (let ((*standard-output* *standard-output*)
                                (*error-output* *error-output*))
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
       (fiveam:run! :lisp-chat-tests)
    (stop-test-server)))

(test tcp-client-connection
       (let ((socket (usocket:socket-connect "127.0.0.1" *port*))
             (stream nil))
         (setf stream (usocket:socket-stream socket))
         (is (not (null socket)))
         ;; Read prompt
         (let ((prompt (read-line stream)))
           (is (search "> Type your username: " prompt)))
         ;; Send username
         (write-line "tester-tcp" stream)
         (finish-output stream)
         ;; Read welcome
         (let ((welcome (read-line stream)))
           (is (search "joined to the party" welcome)))
         (usocket:socket-close socket)))

(test websocket-client-connection
       (let* ((url (format nil "ws://127.0.0.1:~a/ws" *websocket-port*))
              (client (make-client url))
              (connected nil)
              (messages '()))

         (on :open client (lambda () (setf connected t)))
         (on :message client (lambda (msg) (push msg messages)))

         (start-connection client)
         (sleep 1)
         (is (eq t connected))

         ;; Send username (server asks for it first? server sends "> Type your username: ")
         ;; The first message in 'messages' should be the prompt.
         ;; But messages are pushed, so it's in the list.

         ;; Wait a bit for prompt
         (sleep 0.5)
         (is (some (lambda (m) (search "Type your username" m)) messages))

         (send client "tester-ws")
         (sleep 0.5)

         (is (some (lambda (m) (search "joined to the party" m)) messages))

         (close-connection client)))
