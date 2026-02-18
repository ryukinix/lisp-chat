(defpackage :lisp-chat/tui
  (:use #:cl
        #:lisp-chat/config
        #:tuition
        #:bordeaux-threads)
  (:local-nicknames (:vp :tuition.components.viewport)
                    (:ti :tuition.components.textinput))
  (:import-from #:usocket
                #:socket-connect
                #:socket-stream
                #:socket-close)
  (:import-from #:websocket-driver
                #:start-connection
                #:on
                #:close-connection)
  (:import-from #:websocket-driver-client
                #:make-client)
  (:export :main))

(in-package :lisp-chat/tui)

;;; Monkey-patch trivial-channels:recvmsg to avoid lock warnings
(in-package :trivial-channels)
(defun recvmsg (channel &optional timeout)
  (declare (ignore timeout))
  (bt:with-lock-held ((channel-q-mutex channel))
    (loop until (queue-has-item-p (channel-queue channel)) do
      (bt:condition-wait (channel-q-condition channel)
                         (channel-q-mutex channel)))
    (queue-pop (channel-queue channel))))
(in-package :lisp-chat/tui)

;;; Constants and Configuration

(defparameter *colors*
  '("#ff7675" "#fab1a0" "#fdcb6e" "#e17055" "#d63031"
    "#00b894" "#00cec9" "#0984e3" "#6c5ce7" "#e84393"
    "#ffeaa7" "#55efc4" "#81ecec" "#74b9ff" "#a29bfe"))

(defun today ()
  "Returns the current date as a string in YYYY-MM-DD format."
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (declare (ignore second minute hour)) ;; We don't need time, just date
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun hex-to-rgb (hex)
  (let ((hex (string-trim "#" hex)))
    (list (parse-integer hex :start 0 :end 2 :radix 16)
          (parse-integer hex :start 2 :end 4 :radix 16)
          (parse-integer hex :start 4 :end 6 :radix 16))))

(defun render-user-prefix (username)
  (format nil "[~a]: "
          (tui:render-styled
           (tui:make-style :foreground
                           (get-user-color username))
           username)))

(defun get-user-color (username)
  (if (string= username "@server")
      (tui:color-rgb 187 34 34) ; #bb2222
      (let ((hash 0))
        (loop for char across username
              do (setf hash (+ (char-code char) (- (ash hash 12) hash))))
        (let* ((index (mod (abs hash) (length *colors*)))
               (hex (nth index *colors*))
               (rgb (hex-to-rgb hex)))
          (apply #'tui:color-rgb rgb)))))

;;; Model

(defclass chat-model ()
  ((messages :initform nil :accessor messages)
   (viewport :accessor viewport)
   (users-viewport :accessor users-viewport)
   (input :accessor input)
   (socket :initform nil :accessor socket)
   (username :initform nil :accessor username)
   (pending-username :initform nil :accessor pending-username)
   (users :initform nil :accessor users)
   (connected :initform nil :accessor connected)
   (ping-thread :initform nil :accessor ping-thread)
   (last-date :initform nil :accessor last-date)
   (win-width :initform 80 :accessor win-width)
   (win-height :initform 24 :accessor win-height)))

;;; Messages

(defmessage server-msg
  ((text :initarg :text :accessor server-msg-text)))

;;; Helper Functions

(defun websocket-p (host port)
  (declare (ignore port))
  (and (stringp host)
       (or (search "ws://" host) (search "wss://" host))))

(defgeneric connection-send (socket message))

(defmethod connection-send ((socket usocket:usocket) message)
  (write-line message (socket-stream socket))
  (finish-output (socket-stream socket)))

(defmethod connection-send (socket message)
  (websocket-driver:send socket message))

(defun send-message (socket message)
  (connection-send socket message))

(defgeneric read-message (socket))

(defmethod read-message ((socket usocket:usocket))
  (read-line (socket-stream socket) nil :eof))

(defun handle-incoming-message (program msg)
  (when (stringp msg)
    (cond
      (t
       (tui:send program (make-instance 'server-msg :text msg))))))

(defun start-listener (program socket)
  (make-thread
   (lambda ()
     (loop
       (let ((msg (read-message socket)))
         (when (or (null msg) (eq msg :eof))
           (tui:send program (make-instance 'server-msg :text "Disconnected from server."))
           (return))
         (handle-incoming-message program msg))))
   :name "listener-thread"))

(defun start-ping-thread (model)
  (setf (ping-thread model)
        (make-thread
         (lambda ()
           (loop while (connected model)
                 do (sleep 30)
                    (when (connected model)
                      (handler-case
                          (when (username model)
                           (send-message (socket model) "/ping system"))
                        (error (c)
                          (declare (ignore c))
                          (setf (connected model) nil))))))
         :name "ping-thread")))

(defun update-users-list (model)
  (let ((content (with-output-to-string (s)
                   (format s "~a ~a"
                           (tui:render-styled (tui:make-style :foreground (tui:color-rgb 255 255 255)) "Online:")
                           (format nil "~{~a~^, ~}" (sort (copy-list (users model)) #'string<))))))
    (vp:viewport-set-content (users-viewport model) content)))

(defun recalculate-layout (model)
  (let* ((w (win-width model))
         (h (win-height model))
         (input-h 3)
         (users-h 3)
         (viewport-h (max 5 (- h input-h users-h)))
         (new-width (max 10 (- w 2))))
    (setf (vp:viewport-width (viewport model)) w
          (vp:viewport-height (viewport model)) viewport-h
          (vp:viewport-width (users-viewport model)) new-width
          (vp:viewport-height (users-viewport model)) 1 ;; content height (without border)
          (ti:textinput-width (input model)) new-width)
    (update-users-list model)))

;;; TUI Implementation

(defmethod tui:init ((model chat-model))
  (let ((host *host*)
        (port *port*))
    (setf (viewport model) (vp:make-viewport :height 20 :width 60)
          (users-viewport model) (vp:make-viewport :height 1 :width 60)
          (input model) (ti:make-textinput :prompt "> " :placeholder "Type a message..."))

    (recalculate-layout model)

    (handler-case
        (progn
          (if (websocket-p host port)
              (let* ((url (if (or (search "ws://" host) (search "wss://" host))
                             host
                             (format nil "ws://~a:~a/ws" host port)))
                     (client (make-client url)))
                (setf (socket model) client)
                (start-connection client)
                (let ((prog tui:*current-program*))
                  (on :message client
                      (lambda (msg)
                        (handle-incoming-message prog msg))))
                (setf (connected model) t))
              (let ((socket (socket-connect host port)))
                (setf (socket model) socket)
                (setf (connected model) t)
                (start-listener tui:*current-program* socket)))

          (start-ping-thread model))

      (error (c)
        (tui:send tui:*current-program*
                 (make-instance 'server-msg :text (format nil "Connection error: ~a" c)))))
    nil))

(defmethod tui:update-message ((model chat-model) (msg tui:window-size-msg))
  (setf (win-width model) (tui:window-size-msg-width msg)
        (win-height model) (tui:window-size-msg-height msg))
  (recalculate-layout model)
  model)

(defmethod tui:update-message ((model chat-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((or (eq key :enter)
           (string-equal key "enter")
           (and (characterp key) (char= key #\Newline)))
       (let ((text (ti:textinput-value (input model))))
         (when (and text (plusp (length text)))
           (cond
             ((string= text "/quit")
              (setf (connected model) nil)
              (tui:quit tui:*current-program*))
             ((connected model)
              (unless (username model)
                (setf (pending-username model) text))
              (send-message (socket model) text)
              (ti:textinput-reset (input model)))
             (t
              (ti:textinput-reset (input model)))))))
      ((or (string-equal key "escape") (and (string-equal key "c") (tui:key-msg-ctrl msg)))
       (setf (connected model) nil)
       (tui:quit tui:*current-program*))
      (t
       (ti:textinput-update (input model) msg)
       (cond
         ((string-equal key "page-up") (vp:viewport-page-up (viewport model)))
         ((string-equal key "page-down") (vp:viewport-page-down (viewport model)))
         ((string-equal key "up") (vp:viewport-scroll-up (viewport model)))
         ((string-equal key "down") (vp:viewport-scroll-down (viewport model))))))
    model))

(defmethod tui:update-message ((model chat-model) (msg server-msg))
  (dolist (text (tui:split-string-by-newline (server-msg-text msg)))
    (let* ((regex "^\\|(\\d{4}-\\d{2}-\\d{2})? ?(\\d{2}:\\d{2}):(\\d{2})\\| \\[(.*?)\\]: (.*)$")
           (match (multiple-value-list (cl-ppcre:scan-to-strings regex text))))
      (if (first match)
          (let* ((groups (second match))
                 (date (or (aref groups 0)
                           (today)))
                 (time (format nil "~a:~a" (aref groups 1) (aref groups 2)))
                 (user (aref groups 3))
                 (content (aref groups 4))
                 (user-color (get-user-color user))
                 (formatted (format nil "~a [~a]: ~a"
                                    (tui:render-styled (tui:make-style :foreground (tui:color-rgb 100 100 100)) time)
                                    (tui:render-styled (tui:make-style :foreground user-color) user)
                                    content)))

            ;; Process system messages for side-effects
            (cond
              ;; Join
              ((and (string= user "@server") (search "joined to the party!" content))
               (let ((joined-user (subseq content 10 (search " joined" content))))
                 (pushnew joined-user (users model) :test #'string=)
                 (update-users-list model)
                 ;; Check if it's us joining
                 (when (and (null (username model))
                            (pending-username model)
                            (string= joined-user (pending-username model)))
                   (setf (username model) joined-user
                         (pending-username model) nil)
                   (setf (ti:textinput-prompt (input model))
                         (render-user-prefix joined-user))
                   (recalculate-layout model)
                   (send-message (socket model) "/users")
                   (send-message (socket model) "/log 100"))))
              ;; Exit
              ((search "exited from the party :(" content)
               (let ((exited-user (subseq content 10 (search " exited" content))))
                 (setf (users model) (remove exited-user (users model) :test #'string=))
                 (update-users-list model)))
              ;; /users response
              ((and (string= user "@server") (search "users: " content))
               (let* ((list-str (subseq content 7))
                      (users-list (cl-ppcre:split ", " list-str)))
                 (setf (users model) users-list)
                 (update-users-list model)))
              ;; /nick response
              ((and (string= user "@server") (search "Your new nick is: @" content))
               (let ((my-name (subseq content (1+ (search "@" content)))))
                 (setf (users model)
                       (remove (username model) (users model) :test #'string=))
                 (setf (username model) my-name)
                 (setf (ti:textinput-prompt (input model))
                       (render-user-prefix my-name))
                 (pushnew my-name (users model) :test #'string=)
                 (recalculate-layout model)))
               ;; /ping response (ignore)
              ((and (string= user "@server") (search "pong (system)" content))
               ;; Do nothing, just ignore
               nil))

            ;; Only show if not ignored (like pong)
            (unless (and (string= user "@server") (search "pong (system)" content))
              ;; Handle date divider
              (when (and date (not (equal date (last-date model))))
                (setf (last-date model) date)
                (push (tui:render-styled (tui:make-style :foreground (tui:color-rgb 80 80 80))
                                         (format nil "--- ~a ---" date))
                      (messages model)))

              (let ((wrapped (tui:wrap-text formatted (vp:viewport-width (viewport model)) :break-words t)))
                (dolist (line (tui:split-string-by-newline wrapped))
                  (push line (messages model))))

              (vp:viewport-set-content (viewport model) (format nil "~{~a~%~}" (reverse (messages model))))
              (vp:viewport-goto-bottom (viewport model))))
          (progn
             ;; Filter raw pong messages too if they appear without standard formatting
             (unless (search "pong" text)
                (let ((wrapped (tui:wrap-text text (vp:viewport-width (viewport model)) :break-words t)))
                  (dolist (line (tui:split-string-by-newline wrapped))
                    (push line (messages model))))
                (vp:viewport-set-content (viewport model) (format nil "~{~a~%~}" (reverse (messages model))))
                (vp:viewport-goto-bottom (viewport model)))))))
  model)

(defmethod tui:view ((model chat-model))
  (tui:join-vertical
   tui:+left+
   (tui:render-border (vp:viewport-view (users-viewport model)) tui:*border-rounded*)
   (vp:viewport-view (viewport model))
   (tui:render-border (ti:textinput-view (input model)) tui:*border-rounded*)))

(defun main (&key (host *host*) (port *port*))
  (handler-case
      (let ((model (make-instance 'chat-model)))
        (setf *host* host
              *port* port)
        (tui:run (tui:make-program model :alt-screen t :mouse :cell-motion)))
    (error (c)
      (format t "Fatal error: ~a~%" c)
      (uiop:quit 1))))
