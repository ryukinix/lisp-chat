;; Common Lisp Script
;; Manoel Vilela

(defpackage :lisp-chat/client
  (:use #:usocket
        #:cl
        #:lisp-chat/config
        #:bordeaux-threads)
  (:import-from #:websocket-driver
                #:start-connection
                #:send
                #:on
                #:close-connection)
  (:import-from #:websocket-driver-client
                #:make-client)
  (:export :main))

(in-package :lisp-chat/client)

(defvar *io-lock* (make-lock "io mutex")
  "I/O Mutex for avoid terminal race conditions")

(defvar *periodic-ping-interval* 30
  "Interval in seconds to send a ping to the server")

(defvar *stop* nil
  "Stop sign to end the client")

;; WebSocket Support Types
(defstruct safe-queue
  (items '())
  (lock (make-lock "queue-lock"))
  (cvar (make-condition-variable :name "queue-cvar")))

(defun queue-push (q item)
  (with-lock-held ((safe-queue-lock q))
    (setf (safe-queue-items q) (append (safe-queue-items q) (list item)))
    (condition-notify (safe-queue-cvar q))))

(defun queue-pop (q)
  (with-lock-held ((safe-queue-lock q))
    (loop while (null (safe-queue-items q))
          do (condition-wait (safe-queue-cvar q) (safe-queue-lock q)))
    (pop (safe-queue-items q))))

(defstruct ws-connection
  client
  queue)

(defun erase-last-line ()
  "Erase the last line by using ANSI Escape codes"
  (format t "~C[1A~C[2K" #\Esc #\Esc)
  (finish-output))

(defun exit (&optional (code 0))
  #-swank
  (uiop:quit code))

(defun get-user-input (username)
  "Get the user input by using readline"
  (declare (ignore username))
  #+swank (read-line)
  #-swank
  (prog1 (cl-readline:readline :prompt (format nil "[~A]: " username)
                               :erase-empty-line t
                               :add-history t)
    (with-lock-held (*io-lock*)
      (erase-last-line))))

(defun send-message (message socket)
  "Send a MESSAGE string through a SOCKET instance"
  (typecase socket
    (usocket:usocket
     (write-line message (socket-stream socket))
     (finish-output (socket-stream socket)))
    (ws-connection
     (send (ws-connection-client socket) message))))

(defun fetch-message (socket)
  "Fetch a message from the SOCKET (TCP or WS)"
  (typecase socket
    (usocket:usocket
     (read-line (socket-stream socket) nil :eof))
    (ws-connection
     (queue-pop (ws-connection-queue socket)))))

(defun system-pongp (message)
  "SYSTEM-PONGP detect if a pong response was received as systematic send"
  (search "[@server]: pong (system)" message))

;; HACK: I don't know a better way to save state of cl-readline
;; before printing messages from server, so I'm cleaning all the stuff
;; before print a new message, and restore again. Maybe there is a
;; better way for doing that.
#-swank
(defun receive-message (message)
  "Receive a message and print in the terminal carefully with IO race conditions"
  (with-lock-held (*io-lock*)
    (unless (system-pongp message)
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
       (cl-readline:redisplay)))))

#+swank
(defun receive-message (message)
  "Receive a message and print in the terminal carefully with IO race conditions"
  (unless (system-pongp message)
    (format t "~a~%" message)))


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
  (loop for message = (fetch-message socket)
        do (cond
             ((eq message :eof)
              (format t "~%End of connection~%")
              (exit 1))
             ((equal message "/quit")
              (return))
             (t (receive-message message)))))

(defun server-broadcast (socket)
  "Call server-listener treating exceptional cases"
  (handler-case (server-listener socket)
    (simple-error ()
      (server-broadcast socket))))


(defun login (socket)
  "Do the login of the application given a SOCKET instances"
  (let ((msg (fetch-message socket)))
    (if (eq msg :eof)
        (exit 1)
        (princ msg)))
  (finish-output)
  (let ((username (read-line)))
    (send-message username socket)
    username))

(defun client-background-ping (socket)
  "Maintain TCP/IP connection by sending periodic ping to maintain connection alive.

The systematic pong is consumed and the @server response is not shown in the terminal
"
  (loop (sleep *periodic-ping-interval*)
        (ignore-errors
         (send-message "/ping system" socket))))

(defun process-connection (socket host port)
  (let ((username (login socket)))
    (format t "Connected as ~a@~a:~a ~%" username host port)
    (let ((broadcast (make-thread (lambda () (server-broadcast socket))
                                  :name "server broadcast"))
          (background-ping (make-thread (lambda () (client-background-ping socket))
                                        :name "background ping")))
      (client-sender socket username)
      (destroy-thread background-ping)
      (destroy-thread broadcast)
      (typecase socket
        (usocket:usocket (socket-close socket))
        (ws-connection (close-connection (ws-connection-client socket)))))))

(defun client-loop (host port)
  "Dispatch client threads for basic functioning system"
  (if (or (search "ws://" host) (search "wss://" host))
      (let* ((queue (make-safe-queue))
             (client (make-client host))
             (connection (make-ws-connection :client client :queue queue)))
        (on :message client
            (lambda (message)
              (queue-push queue message)))
        (start-connection client)
        (process-connection connection host port))
      (let ((socket (socket-connect host port)))
        (process-connection socket host port))))

(defun main (&key (host *host*) (port *port*))
  "Main function of client"
  (handler-case (client-loop host port)
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal ()
      (exit 0))
    (usocket:connection-refused-error ()
      (progn (write-line "Server seems offline. Run first the server.lisp.")
             (exit 1)))))
