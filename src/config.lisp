;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/config
  (:use :cl)
  (:export :get-version
           :has-any-flags
           :*lisp-command-timeout*
           :*debug*
           :*host*
           :*source-code*
           :*port*
           :*persistence-file*
           :*push-subscriptions-file*
           :*user-sessions-file*
           :*vapid-keys-file*
           :*dm-persistence-file*
           :*dm-cursors-file*
           :*dm-max-messages-per-room*
           :*dm-retention-days*
           :*websocket-port*))

(in-package :lisp-chat/config)

(defparameter *debug* t "Run application in debug mode with extra info in terminal")
(defparameter *host* "0.0.0.0" "Host used in server and client")
(defparameter *port* 5558 "Default port")
(defparameter *websocket-port* 5559 "Web/WebSocket port")
(defparameter *lisp-command-timeout* 2 "Max seconds to timeout command")
(defparameter *source-code* "https://github.com/ryukinix/lisp-chat")
(defparameter *persistence-file* "messages.sexp")
(defparameter *push-subscriptions-file* "push-subscriptions.sexp")
(defparameter *user-sessions-file* "user-sessions.sexp")
(defparameter *vapid-keys-file* "vapid.sexp")
(defparameter *dm-persistence-file* "dm-messages.sexp")
(defparameter *dm-cursors-file* "dm-cursors.sexp")
(defparameter *dm-max-messages-per-room* 200 "Maximum retained DM messages per 1:1 conversation")
(defparameter *dm-retention-days* 30 "Retention window in days for 1:1 DM messages")

(defun get-version ()
  "Get the application version."
  (let ((system (asdf:find-system :lisp-chat)))
    (format nil "~a~a"
            (asdf:component-version system)
            (or (lisp-chat/system:component-build-metadata system) ""))))


(defun has-any-flags (argv &rest flags)
  "Check if any of the given flags are present"
  (loop for flag in flags
        thereis (find flag argv :test #'equal)))
