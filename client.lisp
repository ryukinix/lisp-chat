;; Common Lisp Script
;; Manoel Vilela

(ql:quickload :usocket)

(defpackage :lisp-chat-client
  (:use :usocket :cl)
  (:export :main))

(in-package :lisp-chat-client)
(load "./config.lisp")


(defun erase-last-line ()
  (format t "~C[1A~C[2K" #\Esc #\Esc))


(defun get-user-input ()
  (prog1 (read-line)
         (erase-last-line)))


(defun send-message (message socket)
  (write-line message (socket-stream socket))
  (finish-output (socket-stream socket)))


(defun client-sender (socket)
  (loop for message = (get-user-input)
        do (send-message message socket)
        when (equal message "/quit")
          return nil)
  (sb-ext:exit))


(defun server-listener (socket)
  (loop for message = (read-line (socket-stream socket))
        while (not (equal message "/quit"))
        do (write-line message)))

(defun server-broadcast (socket)
  (handler-case (server-listener socket)
    (end-of-file () (progn (format t "Server down. ~%")
                           (sb-ext:exit)))))


(defun login (socket)
  (princ (read-line (socket-stream socket)))
  (finish-output)
  (let ((username (read-line)))
    (send-message username socket)
    username))


(defun client-loop ()
  (let* ((socket (socket-connect *host* *port*))
         (username (login socket)))
    (format t "Connected as ~a\@~a\:~a ~%" username *host* *port*)
    (let ((sender (sb-thread:make-thread #'client-sender
                                       :name "client sender"
                                       :arguments (list socket)))
          (broadcast (sb-thread:make-thread #'server-broadcast
                                      :name "server broadcast"
                                      :arguments (list socket))))
      (sb-thread:join-thread sender)
      (sb-thread:join-thread broadcast))))

(defun main ()
  (handler-case (client-loop)
    (sb-sys:interactive-interrupt () (sb-ext:exit))
    (usocket:connection-refused-error () (progn (format t "Run first the server.lisp")
                                                (sb-ext:exit :code 1)))))

(main)


