;; Common Lisp Script
;; Manoel Vilela

(ql:quickload :usocket)

(defpackage :lisp-chat
  (:use :usocket :cl))

(in-package :lisp-chat)
(load "./config.lisp")

(defun erase-last-line ()
  (format t "~C[1A~C[2K" #\Esc #\Esc))

(defun get-user-input ()
  (prog1 (read-line)
         (erase-last-line)))

(defun send-message (message socket)
  (write-line message (socket-stream socket))
  (finish-output (socket-stream socket)))


(defun client-read (socket)
  (loop for message = (get-user-input)
        while (not (equal message "/quit"))
        do (send-message message socket))
  (sb-ext:exit))

(defun client-write (socket)
  (loop for message = (read-line (socket-stream socket))
        while (not (equal message "/quit"))
        do (write-line message *standard-output*)))

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
    (let ((reader (sb-thread:make-thread #'client-read :arguments (list socket)))
          (writer (sb-thread:make-thread #'client-write :arguments (list socket))))
      (sb-thread:join-thread reader)
      (sb-thread:join-thread writer))))

(client-loop)
