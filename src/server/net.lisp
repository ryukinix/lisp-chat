(in-package :lisp-chat/server)

(defun push-message (from content)
  "Push a messaged FROM as CONTENT into the *messages-stack*"
  (with-lock-held (*messages-lock*)
    (push (make-message :from from
                        :content content
                        :time (get-time))
          *messages-stack*))
  (signal-semaphore *message-semaphore*))

(defun user-joined-message (client)
  (push-message "@server" (format nil "The user @~a joined to the party!" (client-name client))))

(defun user-exited-message (client)
  (push-message "@server" (format nil "The user @~a exited from the party :(" (client-name client))))

(defun save-message-to-disk (message-raw)
  "Save a single MESSAGE-RAW struct into *persistence-file*"
  (with-open-file (out *persistence-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (let ((*print-readably* t))
      (print message-raw out)
      (finish-output out))))

(defun client-close (client)
  (let ((socket (client-socket client)))
    (typecase socket
      (usocket:stream-usocket
       (socket-close socket))
      (t
       (websocket-driver:close-connection socket)))))

(defun client-delete (client)
  "Delete a CLIENT from the list *clients*"
  (let ((removed? nil))
    (with-lock-held (*client-lock*)
      (when (member client *clients*)
        (setf *clients* (remove client *clients*))
        (setf removed? t)))
    (when removed?
      (user-exited-message client)
      (debug-format t "Deleted user ~a@~a~%"
                    (client-name client)
                    (client-address client))
      (client-close client))))

(defun send-message (client message)
  "Send to CLIENT a MESSAGE :type string"
  (unless (equal message 'ignore)
    (let ((socket (client-socket client)))
      (typecase socket
        (usocket:stream-usocket
         (let ((stream (socket-stream socket)))
           (write-line message stream)
           (finish-output stream)))
        (t
         (websocket-driver:send socket message))))))

(defun message-broadcast ()
  "This procedure is a general independent thread to run brodcasting
   all the clients when a message is ping on this server"
  (loop when (wait-on-semaphore *message-semaphore*)
          do (let* ((message-raw (with-lock-held (*messages-lock*)
                                   (pop *messages-stack*))))
               (when message-raw
                 (let ((message (formatted-message message-raw)))
                   (push message-raw *messages-log*)
                   (save-message-to-disk message-raw)
                   (let ((clients *clients*))
                     (loop for client in clients
                           do (handler-case (send-message client message)
                                (error (e)
                                  (debug-format t "Error broadcasting to ~a: ~a~%" (client-name client) e)
                                  (client-delete client))))))))))

#+sbcl
(sb-alien:define-alien-type tcp-info
  (sb-alien:struct tcp-info
    (state sb-alien:unsigned-char)
    (ca-state sb-alien:unsigned-char)
    (retransmits sb-alien:unsigned-char)
    (probes sb-alien:unsigned-char)
    (backoff sb-alien:unsigned-char)
    (options sb-alien:unsigned-char)
    (wscale sb-alien:unsigned-char)
    (flags sb-alien:unsigned-char)
    (rto sb-alien:unsigned-int)
    (ato sb-alien:unsigned-int)
    (snd-mss sb-alien:unsigned-int)
    (rcv-mss sb-alien:unsigned-int)
    (unacked sb-alien:unsigned-int)
    (sacked sb-alien:unsigned-int)
    (lost sb-alien:unsigned-int)
    (retrans sb-alien:unsigned-int)
    (fackets sb-alien:unsigned-int)
    (last-data-sent sb-alien:unsigned-int)
    (last-ack-sent sb-alien:unsigned-int)
    (last-data-recv sb-alien:unsigned-int)
    (last-ack-recv sb-alien:unsigned-int)
    (pmtu sb-alien:unsigned-int)
    (rcv-ssthresh sb-alien:unsigned-int)
    (rtt sb-alien:unsigned-int)
    (rttvar sb-alien:unsigned-int)))

#+sbcl
(sb-alien:define-alien-routine ("getsockopt" c-getsockopt) sb-alien:int
  (fd sb-alien:int)
  (level sb-alien:int)
  (optname sb-alien:int)
  (optval (* (sb-alien:struct tcp-info)))
  (optlen (* sb-alien:unsigned-int)))

(defun get-stream-fd (stream)
  #+sbcl
  (labels ((get-slot-safe (obj pkg-name sym-name)
             (let ((pkg (find-package pkg-name)))
               (when pkg
                 (let ((sym (find-symbol sym-name pkg)))
                   (when (and sym (slot-exists-p obj sym))
                     (slot-value obj sym))))))
           (unwrap (s)
             (typecase s
               (sb-sys:fd-stream (sb-sys:fd-stream-fd s))
               (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor s))
               (usocket:usocket (unwrap (usocket:socket s)))
               (synonym-stream (unwrap (symbol-value (synonym-stream-symbol s))))
               (two-way-stream (unwrap (two-way-stream-input-stream s)))
               (echo-stream (unwrap (echo-stream-input-stream s)))
               (t (or (let ((inner (get-slot-safe s "FLEXI-STREAMS" "STREAM")))
                        (and inner (unwrap inner)))
                      (let ((inner (get-slot-safe s "CHUNGA" "STREAM")))
                        (and inner (unwrap inner)))
                      (let ((inner (get-slot-safe s "WEBSOCKET-DRIVER.WS.SERVER" "SOCKET")))
                        (and inner (unwrap inner))))))))
    (unwrap stream))
  #-sbcl nil)

(defun client-latency (client)
  "Returns the latency (RTT) and variation (RTTVAR) in microseconds, or NIL if unsupported or failed."
  (if (equal (client-socket-type client) "WebSocket")
      (let ((latency (client-connection-latency client)))
        (if latency
            (values latency 0)
            (values nil nil)))
      #+sbcl
      (let ((fd (get-stream-fd (client-socket client))))
        (when fd
          (sb-alien:with-alien ((info (sb-alien:struct tcp-info))
                                (len sb-alien:unsigned-int (sb-alien:alien-size (sb-alien:struct tcp-info) :bytes)))
            (let ((res (c-getsockopt fd 6 11 (sb-alien:addr info) (sb-alien:addr len))))
              (when (zerop res)
                (values (sb-alien:slot info 'rtt)
                        (sb-alien:slot info 'rttvar)))))))
      #-sbcl nil))
