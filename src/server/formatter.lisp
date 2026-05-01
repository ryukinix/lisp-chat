(in-package :lisp-chat/server)

(defvar *server-nickname* "@server" "The server nickname")
(defvar *raw-command-message* nil "If true, return raw strings instead of formatted-messages")

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

(defun message-time-hour-format (message &optional tz)
  "Format message time to hour format"
  (multiple-value-bind (second minute hour)
      (if tz
          (decode-universal-time (message-universal-time message) tz)
          (values-list (message-time message)))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun message-time-date-format (message &optional tz)
  "Format message time to date format"
  (multiple-value-bind (second minute hour day month year)
      (if tz
          (decode-universal-time (message-universal-time message) tz)
          (values-list (message-time message)))
    (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

(defun clean-channel-char (c)
  (let ((down (char-downcase c)))
    (cond
      ((or (char= down #\Space) (char= down #\_)) #\-)
      ((member down '(#\à #\á #\â #\ã #\ä #\å)) #\a)
      ((member down '(#\è #\é #\ê #\ë)) #\e)
      ((member down '(#\ì #\í #\î #\ï)) #\i)
      ((member down '(#\ò #\ó #\ô #\õ #\ö)) #\o)
      ((member down '(#\ù #\ú #\û #\ü)) #\u)
      ((char= down #\ç) #\c)
      ((char= down #\ñ) #\n)
      ((or (and (char>= down #\a) (char<= down #\z))
           (and (char>= down #\0) (char<= down #\9))
           (char= down #\-))
       down)
      (t nil))))

(defun normalize-channel (channel)
  "Normalize channel name. Ensure it starts with #."
  (when channel
    (let ((c (string-trim '(#\Space #\Return #\Newline #\Tab) channel)))
      (when (uiop:string-prefix-p "%23" c)
        (setf c (subseq c 3)))
      (when (uiop:string-prefix-p "#" c)
        (setf c (subseq c 1)))
      (let ((cleaned (make-array (length c) :element-type 'character :fill-pointer 0)))
        (loop for char across c
              for new-char = (clean-channel-char char)
              when new-char do (vector-push new-char cleaned))
        (if (zerop (length cleaned))
            "#general"
            (concatenate 'string "#" cleaned))))))

(defun format-message-line (time from content)
  (format nil "|~a| [~a]: ~a" time from content))

(defun get-message-by-reference (channel date time user)
  (find-if (lambda (m)
             (and (string-equal (message-channel m) channel)
                  (string-equal (message-from m) user)
                  (string-equal (message-time-date-format m) (format nil "~a ~a" date time))))
           *messages-log*))

(defun get-message-by-reference-string (ref-string)
  "Find a message by its reference string <#channel: YYYY-MM-DD HH:MM:SS [user]>."
  (cl-ppcre:register-groups-bind (channel date time user)
      ("<(#?[A-zÀ-ú0-9_\\-]+):\\s*(\\d{4}-\\d{2}-\\d{2})\\s*(\\d{2}:\\d{2}:\\d{2})\\s*\\[(.*?)\\]>" ref-string)
    (when (and channel date time user)
      (get-message-by-reference channel date time user))))

(defun expand-message-reply (raw-content)
  "Expand message references in the content."
  (let ((content-str raw-content))
    (cl-ppcre:do-scans (start end reg-starts reg-ends
                        "<(#?[A-zÀ-ú0-9_\\-]+):\\s*(\\d{4}-\\d{2}-\\d{2})\\s*(\\d{2}:\\d{2}:\\d{2})\\s*\\[(.*?)\\]>"
                        raw-content)
      (let* ((channel (subseq raw-content (aref reg-starts 0) (aref reg-ends 0)))
             (date (subseq raw-content (aref reg-starts 1) (aref reg-ends 1)))
             (time (subseq raw-content (aref reg-starts 2) (aref reg-ends 2)))
             (user (subseq raw-content (aref reg-starts 3) (aref reg-ends 3)))
             (ref-msg (get-message-by-reference channel date time user)))
        (when ref-msg
          (let* ((original-content (message-content ref-msg))
                 (lines (split original-content :empty-seqs t :delimiterp (lambda (c) (char= c #\Newline))))
                 (quoted-lines (format nil "~{~a~^~%~}"
                                       (loop for line in lines
                                             for i from 0
                                             collect (if (= i 0)
                                                         (format nil "> @~a: ~a" user line)
                                                         (format nil "> ~a" line)))))
                 (replacement (format nil "~a~%~a" quoted-lines (subseq raw-content end))))
            (setf content-str (concatenate 'string (subseq raw-content 0 start) replacement))))))
    content-str))

(defun formatted-message (message &key (client nil) (date-format nil) (global nil))
  "The default message format of this server. MESSAGE is a struct message"
  (let* ((timezone (when client (client-timezone client)))
         (expand-reply (if client (client-expand-reply client) t))
         (time-str (if (string= date-format "date")
                       (message-time-date-format message timezone)
                       (message-time-hour-format message timezone)))
         (from-str (if global
                       (format nil "~a:~a" (message-channel message) (message-from message))
                       (message-from message)))
         (content-str (if expand-reply
                          (expand-message-reply (message-content message))
                          (message-content message)))
         (lines (split content-str :empty-seqs t :delimiterp (lambda (c) (char= c #\Newline)))))
    (format nil "~{~a~^~%~}"
            (mapcar (lambda (line)
                      (format-message-line time-str from-str line))
                    lines))))

(defun user-messages (&key (client nil) (date-format nil) (channel "#general") (global nil))
  "Return only user messages, discard all messages from @server"
  (let* ((messages (remove-if (lambda (m)
                                (string-equal (message-from m) *server-nickname*))
                              *messages-log*))
         (filtered (remove-if-not (lambda (m)
                                    (or global (string-equal (message-channel m) channel)))
                                  messages)))
    (mapcar (lambda (m)
              (formatted-message m :client client :date-format date-format :global global))
            filtered)))

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

(defun search-message (message &key (client nil) (global nil))
  "Format a message for the /search command.
   The user part is prefixed with search:username or channel:username if global."
  (let ((user-part (if global
                       (format nil "~a:~a" (message-channel message) (message-from message))
                       (format nil "search:~a" (message-from message))))
        (timezone (when client (client-timezone client))))
    (format nil "|~a| [~a]: ~a"
            (message-time-date-format message timezone)
            user-part
            (message-content message))))

(defun command-message (content &key (client nil))
  "This function prepare the CONTENT as a message by the @server"
  (if *raw-command-message*
      content
      (let* ((from *server-nickname*)
             (time (get-time))
             (message (make-message :from from :content content :time time)))
        (formatted-message message :client client))))

(defun private-message (client-name content &key (client nil))
  "This function prepare the CONTENT as a message by the @server"
  (let* ((from (format nil "dm:~a" client-name))
         (time (get-time))
         (message (make-message :from from
                                :content content
                                :time time)))
    (formatted-message message :client client)))
