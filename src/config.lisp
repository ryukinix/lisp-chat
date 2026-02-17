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
           :*websocket-port*))

(in-package :lisp-chat/config)

(defparameter *debug* t "Run application in debug mode with extra info in terminal")
(defparameter *host* "0.0.0.0" "Host used in server and client")
(defparameter *port* 5558 "Default port")
(defparameter *websocket-port* 5559 "Web/WebSocket port")
(defparameter *lisp-command-timeout* 2 "Max seconds to timeout command")
(defparameter *source-code* "https://github.com/ryukinix/lisp-chat")
(defparameter *persistence-file* "messages.sexp")

(defun get-version ()
  (let ((system (or (asdf:find-system :lisp-chat nil)
                    (asdf:find-system :lisp-chat-tui nil))))
    (if system
        (format nil "~a~a"
                (asdf:component-version system)
                (or (lisp-chat/system:component-build-metadata system) ""))
        "0.4.0")))


(defun has-any-flags (argv &rest flags)
  (loop for flag in flags
        thereis (find flag argv :test #'equal)))
