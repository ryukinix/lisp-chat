;; Common Lisp Script
;; Manoel Vilela


(asdf/defsystem:defsystem :lisp-chat
  :author "Manoel Vilela"
  :description "An experimental chat irc-like"
  :version "0.1"
  :mailto "manoel_vilela@engineer.com"
  :license "MIT"
  :depends-on ("usocket" "cl-readline")
  :components ((:file "src/config")
               (:file "src/client" :depends-on ("src/config"))
               (:file "src/server" :depends-on ("src/config"))))
