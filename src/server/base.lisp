(in-package :lisp-chat/server)

(defparameter *clients* nil "List of clients")
(defparameter *messages-stack* nil "Messages pending to be send by broadcasting")
(defparameter *messages-log* nil  "Messages log")
(defparameter *user-channels* (make-hash-table :test 'equal) "Mapping of usernames to their last active channel")
(defparameter *private-channels* (make-hash-table :test 'equal) "Set of channels where messages are not saved")
(defparameter *server-nickname* "@server" "The server nickname")
(defvar *raw-command-message* nil "If true, return raw strings instead of formatted-messages")

;; thread control
(defvar *message-semaphore* (bt:make-semaphore :name "message semaphore"
                                            :count 0))
(defvar *client-lock* (bt:make-lock "client list lock"))
(defvar *messages-lock* (bt:make-lock "messages stack lock"))
(defvar *day-names* '("Monday" "Tuesday" "Wednesday"
                      "Thursday" "Friday" "Saturday" "Sunday")
  "Day names")
(defvar *uptime* nil "Uptime of server variable, initialized at server start")


(defstruct message
  "This structure abstract the type message with is saved
   into *messages-log* and until consumed, temporally pushed
   to *messages-stack*. FROM and CONTENT has type string, TIME is a list of decoded time parts."
  from
  content
  time
  (channel "#general"))

(defstruct client
  "This structure handle the creation/control of the clients of the server.
   NAME is a string. Socket is a USOCKET:SOCKET and address is a ipv4 encoded
   string. TIME is a list of decoded time parts since the users is online."
  name
  socket
  address
  time
  (connection-latency nil)
  (user-agent nil)
  (active-channel "#general")
  (session-id (princ-to-string (uuid:make-v4-uuid))))

(defun system-interrupt ()
  #+sbcl 'sb-sys:interactive-interrupt
  #+ccl  'ccl:interrupt-signal-condition
  #+clisp 'system::simple-interrupt-condition
  #+ecl 'ext:interactive-interrupt
  #+allegro 'excl:interrupt-signal)

(defun interrupt-thread-portable (thread)
  (bt:interrupt-thread thread
                       (lambda () (error (system-interrupt)))))

(defun client-socket-type (client)
  "Return the socket type for the given client."
  (typecase (client-socket client)
    (usocket:stream-usocket "TCP")
    (t "WebSocket")))

(defun get-client (client-name)
  "Get client by name"
  (find client-name
        *clients*
        :test (lambda (name client) (equal name (client-name client)))))

(defun get-client-by-session (session-id)
  (find session-id
        *clients*
        :test (lambda (sid client) (string-equal sid (client-session-id client)))))

(defun socket-peer-address (socket)
  "Given a USOCKET:SOCKET instance return a ipv4 encoded IP string"
  (format nil "~{~a~^.~}\:~a"
          (map 'list #'identity (usocket:get-peer-address socket))
          (usocket:get-peer-port socket)))

(defun client-stream (c)
  "Select the stream IO from the client"
  (usocket:socket-stream (client-socket c)))

(defun debug-format (&rest args)
  "If config:*debug* from lisp-chat-config is true, print debug info on
   running based on ARGS"
  (when config:*debug*
      (apply #'format args)))

(defun get-time ()
  "Return a encoded string as HH:MM:SS based on the current timestamp."
  (multiple-value-list (get-decoded-time)))

(defun format-time (time)
  "Format a time list into a string."
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (values-list time)
    (declare (ignore dst-p))
    (format nil
            "~2,'0d:~2,'0d:~2,'0d of ~a, ~4,'0d-~2,'0d-~2,'0d (GMT~@d)"
            hour minute second
            (nth day-of-week *day-names*)
            year month date
            (- tz))))

(defun message-time-hour-format (message)
  "Format message time to hour format"
  (destructuring-bind (second minute hour &rest rest-of-list) (message-time message)
    (declare (ignore rest-of-list))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun message-time-date-format (message)
  "Format message time to date format"
  (destructuring-bind (second minute hour day month year &rest rest-of-list) (message-time message)
    (declare (ignore rest-of-list))
    (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

(defun spacep (c)
  (eql c #\Space))

(defun split-quotation-aware (string delimiterp)
  "Split a string preserving quotation as single tokens"
  (let ((tokens nil)
        (token (make-array 10 :element-type 'character :adjustable t :fill-pointer 0))
        (quote-char nil))
    (loop for char across string
          do (cond
               ((and quote-char (char= char quote-char))
                (setf quote-char nil))
               ((and (not quote-char) (or (char= char #\") (char= char #\')))
                (setf quote-char char))
               ((and (not quote-char) (funcall delimiterp char))
                (when (plusp (length token))
                  (push (copy-seq token) tokens)
                  (setf (fill-pointer token) 0)))
               (t (vector-push-extend char token))))
    (when (plusp (length token))
      (push (copy-seq token) tokens))
    (nreverse tokens)))

(defun split-with-empty-seqs (string delimiterp)
  "Slit a string maintaing empty strings when there is multiple consecutive delimiters"
  (loop for start = 0 then (1+ pos)
        for pos = (position-if delimiterp string :start start)
        collect (subseq string start pos)
        while pos))

(defun split-trivial (string delimiterp)
  (loop for beg = (position-if-not delimiterp string)
          then (position-if-not delimiterp string :start (1+ end))
        for end = (and beg (position-if delimiterp string :start beg))
        when beg
          collect (subseq string beg end)
        while end))

(defun split (string &key (delimiterp #'spacep) quotation-aware empty-seqs)
  "Split a string by a delimiterp function character checking"
  (cond
    ((and quotation-aware empty-seqs)
     (error "QUOTATION-AWARE and WITH-EMTPY-SEQS cannot be used together"))
    (quotation-aware (split-quotation-aware string delimiterp))
    (empty-seqs (split-with-empty-seqs string delimiterp))
    (t (split-trivial string delimiterp))))

(defun startswith (string substring)
  "Check if STRING starts with SUBSTRING."
  (let ((l1 (length string))
        (l2 (length substring)))
    (when (and (> l2 0)
               (>= l1 l2))
      (loop for c1 across string
            for c2 across substring
            always (equal c1 c2)))))

(defun format-message-line (time from content)
  (format nil "|~a| [~a]: ~a" time from content))

(defun formatted-message (message &key (date-format nil))
  "The default message format of this server. MESSAGE is a struct message"
  (let* ((time-str (if (string= date-format "date")
                       (message-time-date-format message)
                       (message-time-hour-format message)))
         (from-str (message-from message))
         (content-str (message-content message))
         (lines (split content-str :empty-seqs t :delimiterp (lambda (c) (char= c #\Newline)))))
    (format nil "~{~a~^~%~}"
            (mapcar (lambda (line)
                      (format-message-line time-str from-str line))
                    lines))))

(defun user-messages (&key (date-format nil) (channel "#general"))
  "Return only user messages, discard all messsages from @server"
  (mapcar (lambda (m) (formatted-message m :date-format date-format))
          (remove-if-not #'(lambda (m) (string-equal (message-channel m) channel))
                     *messages-log*)))

(defun message-universal-time (message)
  "Return the universal time of a message."
  (destructuring-bind (second minute hour date month year &rest rest)
      (message-time message)
    (declare (ignore rest))
    (encode-universal-time second minute hour date month year)))

(defun parse-iso8601 (iso-string)
  "Parse YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD into universal time.
   Expected format: '2026-02-22T14:30:00' or '2026-02-22'"
  (handler-case
      (let* ((parts (uiop:split-string iso-string :separator "T"))
             (date-part (car parts))
             (time-part (cadr parts))
             (date-fields (mapcar #'parse-integer (uiop:split-string date-part :separator "-")))
             (time-fields (if time-part
                              (mapcar #'parse-integer (uiop:split-string time-part :separator ":"))
                              '(0 0 0))))
        (encode-universal-time (or (nth 2 time-fields) 0) ;; second
                               (or (nth 1 time-fields) 0) ;; minute
                               (or (nth 0 time-fields) 0) ;; hour
                               (nth 2 date-fields)        ;; date
                               (nth 1 date-fields)        ;; month
                               (nth 0 date-fields)))      ;; year
    (error () nil)))

(defun search-message (message &key)
  "Format a message for the /search command.
   The user part is prefixed with search:username."
  (format nil "|~a| [search:~a]: ~a"
          (message-time-date-format message)
          (message-from message)
          (message-content message)))

(defun command-message (content)
  "This function prepare the CONTENT as a message by the @server"
  (if *raw-command-message*
      content
      (let* ((from *server-nickname*)
             (time (get-time))
             (message (make-message :from from :content content :time time)))
        (formatted-message message))))

(defun private-message (client-name content)
  "This function prepare the CONTENT as a message by the @server"
  (let* ((from (format nil "dm:~a" client-name))
         (time (get-time))
         (message (make-message :from from
                                :content content
                                :time time)))
    (formatted-message message)))

(defun reset-server ()
  "Reset the server state."
  (bt:with-lock-held (*client-lock*)
    (setf *clients* nil))
  (bt:with-lock-held (*messages-lock*)
    (setf *messages-stack* nil))
  (setf *user-channels* (make-hash-table :test 'equal))
  (setf *private-channels* (make-hash-table :test 'equal))
  (setf *messages-log* nil))

(defun load-persistent-messages ()
  "Load messages from config:*persistence-file* into *messages-log*"
  (when (probe-file config:*persistence-file*)
    (with-open-file (in config:*persistence-file* :direction :input)
      (handler-case
          (let ((msgs (loop for msg = (read in nil :eof)
                            until (eq msg :eof)
                            collect msg)))
            (debug-format t "[info] messages loaded: ~a~%" (length msgs))
            (setq *messages-log* (reverse msgs)))
        (error (e) (debug-format t "Error loading persistence: ~a~%" e))))))
