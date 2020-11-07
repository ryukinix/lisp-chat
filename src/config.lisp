;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/config
  (:use :cl)
  (:export :*debug*
           :*host*
           :*port*))

(in-package :lisp-chat/config)

(defparameter *debug* t "Run application in debug mode with extra info in terminal")
(defparameter *host* "localhost" "Host used in server and client")
(defparameter *port* 5558 "Default port")
