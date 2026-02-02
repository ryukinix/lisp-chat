;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/system
  (:use :cl :asdf :uiop))

(in-package :lisp-chat/system)

(defvar *lisp-chat-author* "Manoel Vilela")
(defvar *lisp-chat-version* "0.3.0")
(defvar *lisp-chat-license* "MIT")

(defsystem :lisp-chat/server
  :author #.*lisp-chat-author*
  :description "An experimental chat irc-like: server"
  :version #.*lisp-chat-version*
  :license #.*lisp-chat-license*
  :depends-on ("usocket"
               "bordeaux-threads"
               "clack"
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

(defsystem :lisp-chat/client
  :author #.*lisp-chat-author*
  :description "An experimental chat irc-like: client"
  :version #.*lisp-chat-version*
  :license #.*lisp-chat-license*
  :depends-on ("usocket"
               "cl-readline"
               "bordeaux-threads"
               "websocket-driver-client")
  :pathname "src"
  :components ((:file "config")
               (:file "client" :depends-on ("config"))))

(defsystem :lisp-chat
  :author #.*lisp-chat-author*
  :description "An experimental chat irc-like"
  :version #.*lisp-chat-version*
  :license #.*lisp-chat-license*
  :depends-on ("lisp-chat/client"
               "lisp-chat/server")
  :in-order-to ((test-op (test-op "lisp-chat/tests"))))

(defsystem :lisp-chat/tests
  :author #.*lisp-chat-author*
  :license #.*lisp-chat-license*
  :depends-on ("lisp-chat/server"
               "lisp-chat/client"
               "fiveam")
  :pathname "tests"
  :components ((:file "integration"))
  :perform (test-op (o c) (symbol-call :lisp-chat/tests :run-tests)))
