;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/system
  (:use :cl :asdf :uiop)
  (:export :*author*
           :*version*
           :*build-metadata*
           :*license*
           :custom-system-class))

(in-package :lisp-chat/system)

(defun lisp-chat-parse-version ()
  (let* ((version (uiop:getenv "APP_VERSION"))
         (index (or (search "-" version)
                    (search "+" version))))
    (if version
        (subseq version 0 index)
        "0.4.0")))

(defun lisp-chat-parse-build-metadata ()
  (let* ((version (uiop:getenv "APP_VERSION"))
         (index (search "-" version)))
    (cond ((and version index) (subseq version index))
          ((and version) "")
          (t "-dev"))))

(defvar *author* "Manoel Vilela")
(defvar *version* (lisp-chat-parse-version))
(defvar *build-metadata* (lisp-chat-parse-build-metadata))
(defvar *license* "MIT")

(defclass custom-system-class (asdf:system)
  ((build-metadata :initarg :build-metadata
                   :accessor component-build-metadata
                   :initform nil)))

(in-package #:asdf-user)

(asdf:defsystem :lisp-chat/server
  :class lisp-chat/system:custom-system-class
  :author #.lisp-chat/system:*author*
  :description "An experimental chat irc-like: server"
  :version #.lisp-chat/system:*version*
  :build-metadata #.lisp-chat/system:*build-metadata*
  :license #.lisp-chat/system:*license*
  :depends-on ("usocket"
               "bordeaux-threads"
               "isolated"
               "clack"
               "clack-handler-hunchentoot"
               "websocket-driver")
  :pathname "src"
  :components ((:file "config")
               (:module "server"
                :depends-on ("config")
                :components ((:file "package")
                             (:file "base" :depends-on ("package"))
                             (:file "commands" :depends-on ("package" "base"))
                             (:file "net" :depends-on ("base"))
                             (:file "tcp" :depends-on ("net" "base" "commands"))
                             (:file "web" :depends-on ("net" "base" "commands"))
                             (:file "main" :depends-on ("tcp" "web" "net"))))
               (:module "static"
                :components
                        ((:static-file "index.html")
                         (:static-file "chat.js")
                         (:static-file "style.css")
                         (:static-file "favicon.ico")
                         (:static-file "logo.png")
                         (:static-file "manifest.json")
                         (:static-file "sw.js")))))

(asdf:defsystem :lisp-chat/client
  :class lisp-chat/system:custom-system-class
  :author #.lisp-chat/system:*author*
  :description "An experimental chat irc-like: client"
  :version #.lisp-chat/system:*version*
  :build-metadata #.lisp-chat/system:*build-metadata*
  :license #.lisp-chat/system:*license*
  :depends-on ("usocket"
               "cl-readline"
               "bordeaux-threads"
               "websocket-driver-client")
  :pathname "src"
  :components ((:file "config")
               (:file "client" :depends-on ("config"))))

(asdf:defsystem :lisp-chat
  :class lisp-chat/system:custom-system-class
  :author #.lisp-chat/system:*author*
  :description "An experimental chat irc-like"
  :version #.lisp-chat/system:*version*
  :build-metadata #.lisp-chat/system:*build-metadata*
  :license #.lisp-chat/system:*license*
  :depends-on ("lisp-chat/client"
               "lisp-chat/server")
  :in-order-to ((test-op (test-op "lisp-chat/tests"))))

(asdf:defsystem :lisp-chat/tests
  :author #.lisp-chat/system:*author*
  :license #.lisp-chat/system:*license*
  :depends-on ("lisp-chat/server"
               "lisp-chat/client"
               "parachute")
  :pathname "tests"
  :components ((:file "integration"))
  :perform (test-op (o c) (symbol-call :lisp-chat/tests :run-tests)))
