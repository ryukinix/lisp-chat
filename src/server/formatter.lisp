(in-package :lisp-chat/server)
(export '(formatted-message normalize-channel message-time-hour-format message-time-date-format))

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

(defun formatted-message (message &key (date-format nil) (global nil))
  "The default message format of this server. MESSAGE is a struct message"
  (let* ((time-str (if (string= date-format "date")
                       (message-time-date-format message)
                       (message-time-hour-format message)))
         (from-str (if global
                       (format nil "~a:~a" (message-channel message) (message-from message))
                       (message-from message)))
         (content-str (message-content message))
         (lines (split content-str :empty-seqs t :delimiterp (lambda (c) (char= c #\Newline)))))
    (format nil "~{~a~^~%~}"
            (mapcar (lambda (line)
                      (format-message-line time-str from-str line))
                    lines))))

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
