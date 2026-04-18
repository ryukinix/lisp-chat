(in-package :lisp-chat/server)
(defparameter *messages-stack* nil "Messages pending to be send by broadcasting")
(defparameter *messages-log* nil  "Messages log")
(defparameter *user-channels* (make-hash-table :test 'equal) "Mapping of usernames to their last active channel")
(defparameter *private-channels* (make-hash-table :test 'equal) "Set of channels where messages are not saved")

;; thread control
(defvar *message-semaphore* (bt:make-semaphore :name "message semaphore"
                                            :count 0))
(defvar *client-lock* (bt:make-lock "client list lock"))
(defvar *messages-lock* (bt:make-lock "messages stack lock"))
(defvar *day-names* '("Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday" "Saturday" "Sunday")
  "Day names")
(defvar *uptime* nil "Uptime of server variable, initialized at server start")

(defun system-interrupt ()
  #+sbcl 'sb-sys:interactive-interrupt
  #+ccl  'ccl:interrupt-signal-condition
  #+clisp 'system::simple-interrupt-condition
  #+ecl 'ext:interactive-interrupt
  #+allegro 'excl:interrupt-signal)

(defun interrupt-thread-portable (thread)
  (bt:interrupt-thread thread
                       (lambda () (error (system-interrupt)))))


(defun debug-format (&rest args)
  "If config:*debug* from lisp-chat-config is true, print debug info on
   running based on ARGS"
  (when config:*debug*
      (apply #'format args)))

(defun get-time ()
  "Return a encoded string as HH:MM:SS based on the current timestamp."
  (multiple-value-list (get-decoded-time)))



(defun reset-server ()
  "Reset the server state."
  (bt:with-lock-held (*client-lock*)
    (setf *clients* nil))
  (bt:with-lock-held (*messages-lock*)
    (setf *messages-stack* nil))
  (setf *user-channels* (make-hash-table :test 'equal))
  (setf *private-channels* (make-hash-table :test 'equal))
  (setf *messages-log* nil))
