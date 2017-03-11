;; Common Lisp Script
;; Manoel Vilela


(asdf/defsystem:defsystem :lisp-chat
  :author "Manoel Vilela"
  :email "manoel_vilela@engineer.com"
  :version "0.1"
  :license "MIT"
  :depends-on ("usocket")
  :components ((:file "config")
               (:file "client" :depends-on ("config"))
               (:file "server" :depends-on ("config"))))
