(in-package :lisp-chat/tests)

(define-test lisp-chat-tests
  :description "Main test suite for lisp-chat")

(defvar *server-thread* nil)
(defparameter *print-names* t)
(defparameter *port* 9998)
(defparameter *debug* nil)
(defparameter *websocket-port* 9999)
(defparameter *lisp-command-timeout* 0.5)

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
