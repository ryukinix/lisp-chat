;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/client
  (:use #:usocket
        #:cl
        #:lisp-chat/config
        #:bordeaux-threads)
  (:export :main))

(in-package :lisp-chat/client)

(defvar *io-lock* (make-lock "io mutex")
  "I/O Mutex for avoid terminal race conditions")

(defun erase-last-line ()
  "Erase the last line by using ANSI Escape codes"
  (format t "~C[1A~C[2K" #\Esc #\Esc))


(defun get-user-input (username)
  "Get the user input by using readline"
  (prog1 (cl-readline:readline :prompt (format nil "[~A]: " username)
                               :erase-empty-line t
                               :add-history t)
    (with-mutex-held (*io-lock*)
      (erase-last-line))))


(defun send-message (message socket)
  "Send a MESSAGE string through a SOCKET instance"
  (write-line message (socket-stream socket))
  (finish-output (socket-stream socket)))

;; HACK: I don't know a better way to save state of cl-readline
;; before printing messages from server, so I'm cleaning all the stuff
;; before print a new message, and restore again. Maybe there is a
;; better way for doing that.
(defun receive-message (message)
  "Receive a message and print in the terminal carefully with IO race conditions"
  (with-mutex-held (*io-lock*)
    (let ((line cl-readline:*line-buffer*)
          (prompt cl-readline:+prompt+))
      ;; erase
      (cl-readline:replace-line "" nil)
      (cl-readline:set-prompt "")
      (cl-readline:redisplay)
      ;; print message from server
      (write-line message)
      ;; restore
      (cl-readline:replace-line (or line "") nil)
      (setq cl-readline:*point* cl-readline:+end+)
      (cl-readline:set-prompt prompt)
      (cl-readline:redisplay))))

(defun client-sender (socket username)
  "Routine to check new messages being typed by the user"
  (loop for message = (get-user-input username)
        when (or (equal message "/quit")
                 (equal message nil))
          return nil
        do (send-message message socket))
  (exit))


(defun server-listener (socket)
  "Routine to check new messages coming from the server"
  (loop for message = (read-line (socket-stream socket))
        while (not (equal message "/quit"))
        do (receive-message message)))

(defun server-broadcast (socket)
  "Call server-listener treating exceptional cases"
  (handler-case (server-listener socket)
    (end-of-file (e)
      (format t "~%Server down. ~%")
      (exit))
    (simple-error (e)
      (server-broadcast socket))))


(defun login (socket)
  "Do the login of the application given a SOCKET instances"
  (princ (read-line (socket-stream socket)))
  (finish-output)
  (let ((username (read-line)))
    (send-message username socket)
    username))


(defun client-loop (host port)
  "Dispatch client threads for basic functioning system"
  (let* ((socket (socket-connect host port))
         (username (login socket)))
    (format t "Connected as ~a\@~a\:~a ~%" username *host* *port*)
    (let ((sender (make-thread (lambda () (client-sender socket username))
                               :name "client sender"))
          (broadcast (make-thread (lambda () (server-broadcast socket))
                                  :name "server broadcast")))
      (join-thread sender)
      (join-thread broadcast))))

(defun main (&key (host *host*) (port *port*))
  "Main function of client"
  (handler-case (client-loop host port)
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal ()
      (uiop:quit 0))
    (usocket:connection-refused-error ()
      (progn (write-line "Server seems offline. Run first the server.lisp.")
             (uiop:quit 1)))))
