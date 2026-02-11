(defpackage :lisp-chat/tests
  (:use :cl :parachute)
  (:import-from :lisp-chat/config :*port* :*websocket-port* :*host* :*debug* :*lisp-command-timeout*)
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
(defparameter *lisp-command-timeout* 0.5)

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

;;; Declarative Testing Helpers

(defmacro with-tcp-client ((stream) &body body)
  (let ((socket (gensym)))
    `(let* ((,socket (usocket:socket-connect "127.0.0.1" *port*))
            (,stream (usocket:socket-stream ,socket)))
       (unwind-protect
            (progn ,@body)
         (usocket:socket-close ,socket)))))

(defun tcp-interaction (stream &rest steps)
  "Execute a sequence of steps:
   - string: send line to server
   - (:expect pattern): wait and check if line contains pattern
   - (:ignore [n]): read and discard n lines (default 1)
   - (:sleep n): sleep for n seconds"
  (dolist (step steps)
    (cond
      ((stringp step)
       (write-line step stream)
       (finish-output stream))
      ((and (listp step) (eq (car step) :expect))
       (let ((line (read-line stream)))
         (when *debug* (format t "RECV: ~A~%" line))
         (true (search (second step) line))))
      ((and (listp step) (eq (car step) :ignore))
       (loop repeat (or (second step) 1) do (read-line stream)))
      ((and (listp step) (eq (car step) :sleep))
       (sleep (second step))))))

(defun ws-interaction (client messages-fn &rest steps)
  (dolist (step steps)
    (cond
      ((stringp step)
       (send client step))
      ((and (listp step) (eq (car step) :expect))
       (let ((pattern (second step))
             (found nil))
         ;; Poll for message
         (loop repeat 30 do
           (when (some (lambda (m) (search pattern m)) (funcall messages-fn))
             (setf found t)
             (return))
           (sleep 0.1))
         (true found "Pattern '~A' not found. Messages received: ~S" pattern (funcall messages-fn))))
      ((and (listp step) (eq (car step) :sleep))
       (sleep (second step))))))

;;; Tests

(define-test tcp-client-connection
  :parent lisp-chat-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-tcp"
      '(:expect "The user @tester-tcp joined to the party!"))))

(define-test websocket-client-connection
  :parent lisp-chat-tests
  (let* ((url (format nil "ws://127.0.0.1:~a/ws" *websocket-port*))
         (client (make-client url))
         (connected nil)
         (messages '()))
    (on :open client (lambda () (setf connected t)))
    (on :message client (lambda (msg) (push msg messages)))
    (start-connection client)
    ;; Wait for connection
    (loop repeat 20 until connected do (sleep 0.1))
    (true connected)
    (ws-interaction client (lambda () messages)
      '(:expect "Type your username")
      "tester-ws"
      '(:expect "The user @tester-ws joined to the party!"))
    (close-connection client)))

(define-test log-commands-with-date-format
  :parent lisp-chat-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-log"
      '(:ignore 1) ;; welcome message
      "hello log"
      '(:sleep 0.5)
      '(:ignore 1) ;; broadcast of "hello log"
      "/log :date-format date"
      `(:expect ,(get-current-date)))))

(define-test lisp-command-with-timeout
  :parent lisp-chat-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-lisp"
      '(:ignore 1) ;; welcome message
      "/lisp (dotimes (x 1000) (when (eq x 2) (setq x 1)))"
      '(:sleep 1)
      '(:expect "TIMEOUT: Timeout occurred after 0.5 seconds"))))
