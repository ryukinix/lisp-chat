(defpackage :lisp-chat/client/net
  (:use #:cl)
  (:local-nicknames (#:config #:lisp-chat/config))
  (:import-from #:usocket
                #:socket-stream
                #:socket-close)
  (:import-from #:websocket-driver
                #:send
                #:close-connection)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-condition-variable
                #:condition-notify
                #:condition-wait)
  (:export #:websocket-p
           #:connection-send
           #:connection-read
           #:connection-close
           #:safe-queue
           #:make-safe-queue
           #:queue-push
           #:queue-pop
           #:ws-connection
           #:make-ws-connection
           #:ws-connection-client
           #:ws-connection-queue
           #:*client-type*
           #:make-client))

(in-package :lisp-chat/client/net)

(defvar *client-type* nil)

(defun user-agent-string ()
  (format nil "LispChat/~a (~a; ~a; ~a) ~a/~a"
          (config:get-version)
          (software-type)
          (machine-type)
          *client-type*
          (lisp-implementation-type)
          (lisp-implementation-version)))

(defun make-client (url &rest args)
  "Make client for url"
  (let ((user-agent (user-agent-string)))
    (if (getf args :additional-headers)
        (setf (getf args :additional-headers)
              (acons "User-Agent" user-agent (getf args :additional-headers)))
        (setf (getf args :additional-headers)
              `(("User-Agent" . ,user-agent))))
    (apply #'websocket-driver-client:make-client url args)))

(defstruct safe-queue
  (items '())
  (lock (bt:make-lock "queue-lock"))
  (cvar (make-condition-variable :name "queue-cvar")))

(defun queue-push (q item)
  "Push to queue"
  (bt:with-lock-held ((safe-queue-lock q))
    (setf (safe-queue-items q) (append (safe-queue-items q) (list item)))
    (condition-notify (safe-queue-cvar q))))

(defun queue-pop (q)
  "Pop from queue"
  (bt:with-lock-held ((safe-queue-lock q))
    (loop while (null (safe-queue-items q))
          do (condition-wait (safe-queue-cvar q) (safe-queue-lock q)))
    (pop (safe-queue-items q))))

(defstruct ws-connection
  client
  queue)

(defun websocket-p (host port)
  "Check if websocket"
  (declare (ignore port))
  (and (stringp host)
       (or (search "ws://" host) (search "wss://" host))))

(defgeneric connection-send (socket message)
  (:documentation "Send over connection"))

(defmethod connection-send ((socket usocket:usocket) message)
  (write-line message (usocket:socket-stream socket))
  (finish-output (usocket:socket-stream socket)))

(defmethod connection-send ((socket ws-connection) message)
  (websocket-driver:send (ws-connection-client socket) message))

(defgeneric connection-read (socket)
  (:documentation "Read from connection"))

(defmethod connection-read ((socket usocket:usocket))
  (read-line (usocket:socket-stream socket) nil :eof))

(defmethod connection-read ((socket ws-connection))
  (queue-pop (ws-connection-queue socket)))

(defgeneric connection-close (socket)
  (:documentation "Close connection"))

(defmethod connection-close ((socket usocket:usocket))
  (usocket:socket-close socket))

(defmethod connection-close ((socket ws-connection))
  (websocket-driver:close-connection (ws-connection-client socket)))
