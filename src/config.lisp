;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/config
  (:use :cl)
  (:export :get-version
           :has-any-flags
           :*lisp-command-timeout*
           :*debug*
           :*host*
           :*port*
           :*websocket-port*)
  (:import-from :lisp-chat/system :component-build-metadata))

(in-package :lisp-chat/config)

(defparameter *debug* t "Run application in debug mode with extra info in terminal")
(defparameter *host* "0.0.0.0" "Host used in server and client")
(defparameter *port* 5558 "Default port")
(defparameter *websocket-port* 5559 "Web/WebSocket port")
(defparameter *lisp-command-timeout* 2 "Max seconds to timeout command")

(defun get-version ()
  (let ((system (asdf:find-system :lisp-chat)))
    (format nil "~a~a"
            (asdf:component-version system)
            (or (component-build-metadata system) ""))))


(defun has-any-flags (argv &rest flags)
  (loop for flag in flags
        thereis (find flag argv :test #'equal)))
