(in-package :lisp-chat/tests)

(define-test lisp-chat-tests
  :description "Main test suite for lisp-chat")

(defvar *server-thread* nil)
(defparameter *print-names* t)
(defparameter config:*port* 9996)
(defparameter config:*debug* (or (uiop:getenv "DEBUG")
                          (uiop:getenv "ACTIONS_STEP_DEBUG")))
(defparameter config:*websocket-port* 9997)
(defparameter config:*lisp-command-timeout* 0.5)
(defparameter config:*persistence-file* "messages.test.sexp")


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
    (interrupt-thread-portable *server-thread*))
  (setf *server-thread* nil)
  (sleep 1))

(defun run-tests ()
  (start-test-server)
  (unwind-protect
       (let ((report (parachute:test 'lisp-chat-tests)))
         (when (eq (parachute:status report) :failed)
           (uiop:quit 1))
         report)
    (stop-test-server)))
