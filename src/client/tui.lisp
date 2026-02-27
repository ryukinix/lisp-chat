(defpackage :lisp-chat/tui
  (:use #:cl
        #:lisp-chat/config
        #:lisp-chat/client/net
        #:tuition
        #:bordeaux-threads)
  (:local-nicknames (#:vp #:tuition.components.viewport)
                    (#:ti #:tuition.components.textinput))
  (:import-from #:usocket
                #:socket-connect)
  (:import-from #:websocket-driver
                #:start-connection
                #:on)
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

;;; Monkey-patch tuition:get-terminal-size to avoid poll(2) inside signal handler
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-alien:define-alien-type winsize
      (sb-alien:struct winsize
                       (ws-row sb-alien:unsigned-short)
                       (ws-col sb-alien:unsigned-short)
                       (ws-xpixel sb-alien:unsigned-short)
                       (ws-ypixel sb-alien:unsigned-short)))

  (sb-alien:define-alien-routine ("ioctl" ioctl-get-winsize) sb-alien:int
    (fd sb-alien:int)
    (request sb-alien:unsigned-long)
    (arg (* (sb-alien:struct winsize))))

  (defun sbcl-get-terminal-size ()
    (sb-alien:with-alien ((ws (sb-alien:struct winsize)))
      (if (zerop (ioctl-get-winsize 1 #x5413 (sb-alien:addr ws)))
          (cons (sb-alien:slot ws 'ws-col)
                (sb-alien:slot ws 'ws-row))
          (cons 80 24))))

  (defun tuition::get-terminal-size ()
    (sbcl-get-terminal-size)))

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

(defun to-int32 (x)
  (let ((val (logand x #xFFFFFFFF)))
    (if (logbitp 31 val)
        (- val #x100000000)
        val)))

(defun get-user-color (username)
  (if (string= username "@server")
      (tui:color-rgb 187 34 34) ; #bb2222
      (let ((hash 0))
        (loop for char across username
              do (let ((shifted (to-int32 (ash (to-int32 hash) 12))))
                   (setf hash (+ (char-code char) (- shifted hash)))))
        (let* ((index (mod (abs hash) (length *colors*)))
               (hex (nth index *colors*))
               (rgb (hex-to-rgb hex)))
          (apply #'tui:color-rgb rgb)))))

(defun colorize-mentions (content)
  "Colorizes citations like @user in the message content."
  (cl-ppcre:regex-replace-all "@[a-zA-Z0-9_.-]+" content
    (lambda (mention)
      (let ((username (if (string= mention "@server")
                          mention
                          (subseq mention 1))))
        (tui:render-styled
         (tui:make-style :foreground (get-user-color username))
         mention)))
    :simple-calls t))

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

(defun handle-incoming-message (program msg)
  (when (stringp msg)
    (cond
      (t
       (tui:send program (make-instance 'server-msg :text msg))))))

(defun start-listener (program socket)
  (make-thread
   (lambda ()
     (loop
       (let ((msg (connection-read socket)))
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
                           (connection-send (socket model) "/ping system"))
                        (error (c)
                          (declare (ignore c))
                          (setf (connected model) nil))))))
         :name "ping-thread")))

(defun update-users-list (model)
  (let* ((sorted-users (sort (copy-list (users model)) #'string<))
         (colored-users (mapcar (lambda (u)
                                  (tui:render-styled
                                   (tui:make-style :foreground (get-user-color u))
                                   u))
                                sorted-users))
         (content (with-output-to-string (s)
                    (format s "~a ~a"
                            (tui:render-styled (tui:make-style :foreground (tui:color-rgb 255 255 255)) "Online:")
                            (format nil "~{~a~^, ~}" colored-users)))))
    (vp:viewport-set-content (users-viewport model) content)))

(defun render-messages (model &optional (at-bottom-p nil at-bottom-provided-p))
  (let ((w (vp:viewport-width (viewport model)))
        (at-bottom-p (if at-bottom-provided-p
                         at-bottom-p
                         (vp:viewport-at-bottom-p (viewport model))))
        (rendered-lines nil))
    (dolist (msg (reverse (messages model)))
      (cond
        ((eq (first msg) :date)
         (let* ((date (second msg))
                (date-str (format nil "--- ~a ---" date))
                (pad (max 0 (floor (- w (length date-str)) 2)))
                (padding (make-string pad :initial-element #\Space)))
           (push (tui:render-styled (tui:make-style :foreground (tui:color-rgb 80 80 80))
                                    (format nil "~a~a" padding date-str))
                 rendered-lines)))
        ((eq (first msg) :msg)
         (let ((wrapped (tui:wrap-text (second msg) w :break-words t)))
           (dolist (line (tui:split-string-by-newline wrapped))
             (push line rendered-lines))))))
    (vp:viewport-set-content (viewport model) (format nil "~{~a~^~%~}" (reverse rendered-lines)))
    (when at-bottom-p
      (vp:viewport-goto-bottom (viewport model)))))

(defun recalculate-layout (model)
  (let* ((w (win-width model))
         (h (win-height model))
         (input-h 3)
         (users-h 3)
         (viewport-h (max 5 (- h input-h users-h)))
         (new-width (max 10 (- w 2)))
         (prompt-len (tui:visible-length (ti:textinput-prompt (input model))))
         (at-bottom-p (vp:viewport-at-bottom-p (viewport model))))
    (setf (vp:viewport-width (viewport model)) w
          (vp:viewport-height (viewport model)) viewport-h
          (vp:viewport-width (users-viewport model)) new-width
          (vp:viewport-height (users-viewport model)) 1 ;; content height (without border)
          (ti:textinput-width (input model)) (max 1 (- new-width prompt-len)))
    (update-users-list model)
    (render-messages model at-bottom-p)))

;;; TUI Implementation

(defun connect-websocket (model host port)
  (let* ((url (if (or (search "ws://" host) (search "wss://" host))
                 host
                 (format nil "ws://~a:~a/ws" host port)))
         (client (make-client url))
         (queue (make-safe-queue))
         (connection (make-ws-connection :client client :queue queue)))
    (setf (socket model) connection)
    (on :message client
        (lambda (msg)
          (queue-push queue msg)))
    (on :close client
        (lambda (&key code reason)
          (declare (ignore code reason))
          (queue-push queue :eof)))
    (start-connection client)
    (setf (connected model) t)
    (start-listener tui:*current-program* connection)))

(defun connect-tcp (model host port)
  (let ((socket (socket-connect host port)))
    (setf (socket model) socket)
    (setf (connected model) t)
    (start-listener tui:*current-program* socket)))

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
              (connect-websocket model host port)
              (connect-tcp model host port))

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

(defun handle-chat-input (model text)
  "Processes the text input from the user."
  (when (and text (plusp (length text)))
    (cond
      ((string= text "/quit")
       (setf (connected model) nil)
       (tui:quit tui:*current-program*))
      ((string= text "/clear")
       (setf (messages model) nil)
       (setf (last-date model) nil)
       (render-messages model)
       (ti:textinput-reset (input model)))
      ((connected model)
       (unless (username model)
         (setf (pending-username model) text))
       (connection-send (socket model) text)
       (vp:viewport-goto-bottom (viewport model))
       (ti:textinput-reset (input model)))
      (t
       (ti:textinput-reset (input model))))))

(defun handle-viewport-navigation (model key)
  "Handles viewport scrolling based on key input."
  (cond
    ((string-equal key "page-up") (vp:viewport-page-up (viewport model)))
    ((string-equal key "page-down") (vp:viewport-page-down (viewport model)))
    ((string-equal key "up") (vp:viewport-scroll-up (viewport model)))
    ((string-equal key "down") (vp:viewport-scroll-down (viewport model)))))

(defmethod tui:update-message ((model chat-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((or (eq key :enter)
           (string-equal key "enter")
           (and (characterp key) (char= key #\Newline)))
       (handle-chat-input model (ti:textinput-value (input model))))
      ((or (string-equal key "escape") (and (string-equal key "c") (tui:key-msg-ctrl msg)))
       (setf (connected model) nil)
       (tui:quit tui:*current-program*))
      (t
       (ti:textinput-update (input model) msg)
       (handle-viewport-navigation model key))))
  model)

(defun process-system-message (model user content)
  "Handles server-side events like user joins, exits, and commands."
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
         (connection-send (socket model) "/users")
         (connection-send (socket model) "/log 100"))))
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
       (recalculate-layout model)))))

(defun format-chat-message (time user content)
  "Returns a styled string for displaying a chat message."
  (format nil "~a [~a]: ~a"
          (tui:render-styled (tui:make-style :foreground (tui:color-rgb 100 100 100)) time)
          (tui:render-styled (tui:make-style :foreground (get-user-color user)) user)
          (colorize-mentions content)))

(defmethod tui:update-message ((model chat-model) (msg server-msg))
  (let ((should-render nil))
    (dolist (text (tui:split-string-by-newline (server-msg-text msg)))
      (let* ((regex "^\\|(\\d{4}-\\d{2}-\\d{2})? ?(\\d{2}:\\d{2}):(\\d{2})\\| \\[(.*?)\\]: (.*)$")
             (match (multiple-value-list (cl-ppcre:scan-to-strings regex text))))
        (if (first match)
            (let* ((groups (second match))
                   (date (or (aref groups 0)
                             (today)))
                   (time (format nil "~a:~a" (aref groups 1) (aref groups 2)))
                   (user (aref groups 3))
                   (content (aref groups 4)))

              ;; Process system messages for side-effects
              (process-system-message model user content)

              ;; Only show if not ignored (like pong)
              (unless (and (string= user "@server") (search "pong (system)" content))
                ;; Handle date divider
                (when (and date (not (equal date (last-date model))))
                  (setf (last-date model) date)
                  (push (list :date date) (messages model)))

                (push (list :msg (format-chat-message time user content)) (messages model))
                (setf should-render t)))
            (progn
               ;; Filter raw pong messages too if they appear without standard formatting
               (unless (search "pong" text)
                  (let ((colored-text (colorize-mentions text)))
                    (push (list :msg colored-text) (messages model))
                    (setf should-render t)))))))
    (when should-render
      (render-messages model)))
  model)

(defmethod tui:view ((model chat-model))
  (tui:join-vertical
   tui:+left+
   (tui:render-border (vp:viewport-view (users-viewport model)) tui:*border-rounded*)
   (vp:viewport-view (viewport model))
   (tui:render-border (ti:textinput-view (input model)) tui:*border-rounded*)))

(defun main (&key (host *host*) (port *port*))
  (setf *client-type* "TUI")
  (handler-case
      (let ((model (make-instance 'chat-model)))
        (setf *host* host
              *port* port)
        (tui:run (tui:make-program model :alt-screen t :mouse :cell-motion)))
    (error (c)
      (format t "Fatal error: ~a~%" c)
      (uiop:quit 1))))
