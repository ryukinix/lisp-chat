;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/system
  (:use :cl :asdf :uiop)
  (:export :*author*
           :*version*
           :*build-metadata*
           :*license*
           :custom-system-class
           :component-build-metadata))

(in-package :lisp-chat/system)

(defun version-separator (version)
  (or (search "-" version)
      (search "+" version)))

(defun lisp-chat-parse-version (version)
  (let ((index (version-separator version)))
    (if version
        (subseq version 0 index)
        "0.4.0")))

(defun lisp-chat-parse-build-metadata (version)
  (let ((index (version-separator version)))
    (cond ((and version index) (subseq version index))
          ((and version) "")
          (t "-dev"))))

(defvar *author* "Manoel Vilela")
(defvar *version* (lisp-chat-parse-version (uiop:getenv "APP_VERSION")))
(defvar *build-metadata* (lisp-chat-parse-build-metadata (uiop:getenv "APP_VERSION")))
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
               "tuition"
               "cl-ppcre"
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
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "unit" :depends-on ("main"))
               (:file "integration" :depends-on ("main")))
  :perform (test-op (o c) (symbol-call :lisp-chat/tests :run-tests)))
