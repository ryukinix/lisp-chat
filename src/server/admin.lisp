(defpackage #:lisp-chat/admin
  (:use #:cl #:lisp-chat/server #:lisp-chat/config)
  (:export #:main))

(in-package #:lisp-chat/admin)

(defun load-messages (&optional (file *persistence-file*))
  (when (probe-file file)
    (let ((*package* (find-package :lisp-chat/server)))
      (with-open-file (in file :direction :input)
        (loop for msg = (read in nil :eof)
              until (eq msg :eof)
              collect msg)))))

(defun save-messages (messages &optional (file *persistence-file*))
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-readably* t))
      (dolist (msg messages)
        (print msg out)
        (terpri out)))))

(defun delete-channel (channel)
  (let* ((messages (load-messages))
         (filtered (remove-if (lambda (m) (equal (message-channel m) channel))
                              messages)))
    (save-messages filtered)
    (format t "Deleted channel ~a and its messages (~d messages removed).~%"
            channel (- (length messages) (length filtered)))))

(defun delete-user-messages (user)
  (let* ((messages (load-messages))
         (filtered (remove-if (lambda (m) (equal (message-from m) user))
                              messages)))
    (save-messages filtered)
    (format t "Deleted all messages from user ~a (~d messages removed).~%"
            user (- (length messages) (length filtered)))))

(defun rename-channel (old-name new-name)
  (let* ((messages (load-messages))
         (changed 0))
    (dolist (m messages)
      (when (equal (message-channel m) old-name)
        (setf (message-channel m) new-name)
        (incf changed)))
    (save-messages messages)
    (format t "Renamed channel ~a to ~a (~d messages updated).~%"
            old-name new-name changed)))

(defun rename-user (old-name new-name)
  (let* ((messages (load-messages))
         (changed 0))
    (dolist (m messages)
      (when (equal (message-from m) old-name)
        (setf (message-from m) new-name)
        (incf changed)))
    (save-messages messages)
    (format t "Renamed user ~a to ~a (~d messages updated).~%"
            old-name new-name changed)))

(defun search-messages (query)
  (let ((messages (load-messages)))
    (dolist (m messages)
      (when (search query (message-content m) :test #'char-equal)
        (format t "~a~%" (search-message m))))))

(defun show-history (&key channel user limit)
  (let ((messages (load-messages)))
    (when channel
      (setf messages (remove-if-not (lambda (m) (equal (message-channel m) channel)) messages)))
    (when user
      (setf messages (remove-if-not (lambda (m) (equal (message-from m) user)) messages)))
    (when limit
      (setf messages (last messages limit)))
    (dolist (m messages)
      (format t "~a~%" (formatted-message m :date-format "date")))))

(defun stats ()
  (let ((messages (load-messages))
        (user-counts (make-hash-table :test 'equal))
        (channel-counts (make-hash-table :test 'equal))
        (hour-counts (make-hash-table))
        (total 0))
    (setf total (length messages))
    (dolist (m messages)
      (incf (gethash (message-from m) user-counts 0))
      (incf (gethash (message-channel m) channel-counts 0))
      (incf (gethash (nth 2 (message-time m)) hour-counts 0)))
    (format t "Total messages: ~d~%" total)
    (format t "~%Messages by user:~%")
    (let ((users nil))
      (maphash (lambda (k v) (push (cons k v) users)) user-counts)
      (dolist (u (sort users #'> :key #'cdr))
        (format t "  ~20a: ~d~%" (car u) (cdr u))))
    (format t "~%Messages by channel:~%")
    (let ((channels nil))
      (maphash (lambda (k v) (push (cons k v) channels)) channel-counts)
      (dolist (c (sort channels #'> :key #'cdr))
        (format t "  ~20a: ~d~%" (car c) (cdr c))))
    (format t "~%Messages by hour (UTC):~%")
    (loop for hour from 0 to 23
          do (let ((count (gethash hour hour-counts 0)))
               (when (> count 0)
                 (format t "  ~2,'0d:00 - ~2,'0d:59: ~d~%" hour hour count))))))

;; CLI implementation using clingon

(defun top-level-handler (cmd)
  (clingon:print-usage cmd t))

(defun delete-channel-handler (cmd)
  (let ((channel (clingon:getopt cmd :name)))
    (if channel
        (delete-channel channel)
        (format t "Error: channel name required.~%"))))

(defun delete-user-handler (cmd)
  (let ((user (clingon:getopt cmd :name)))
    (if user
        (delete-user-messages user)
        (format t "Error: user name required.~%"))))

(defun rename-channel-handler (cmd)
  (let ((old (clingon:getopt cmd :old))
        (new (clingon:getopt cmd :new)))
    (if (and old new)
        (rename-channel old new)
        (format t "Error: old and new names required.~%"))))

(defun rename-user-handler (cmd)
  (let ((old (clingon:getopt cmd :old))
        (new (clingon:getopt cmd :new)))
    (if (and old new)
        (rename-user old new)
        (format t "Error: old and new names required.~%"))))

(defun search-handler (cmd)
  (let ((query (clingon:getopt cmd :query)))
    (if query
        (search-messages query)
        (format t "Error: search query required.~%"))))

(defun history-handler (cmd)
  (let ((channel (clingon:getopt cmd :channel))
        (user (clingon:getopt cmd :user))
        (limit (clingon:getopt cmd :limit)))
    (show-history :channel channel :user user :limit limit)))

(defun stats-handler (cmd)
  (declare (ignore cmd))
  (stats))

(defun make-admin-command ()
  (clingon:make-command
   :name "lisp-chat-admin"
   :description "Admin tools for lisp-chat"
   :handler #'top-level-handler
   :sub-commands (list
                  (clingon:make-command
                   :name "delete-channel"
                   :description "Delete a channel and its messages"
                   :options (list
                             (clingon:make-option
                              :string
                              :description "channel name"
                              :short-name #\c
                              :long-name "name"
                              :key :name))
                   :handler #'delete-channel-handler)
                  (clingon:make-command
                   :name "delete-user"
                   :description "Delete all messages of a specific user"
                   :options (list
                             (clingon:make-option
                              :string
                              :description "user name"
                              :short-name #\u
                              :long-name "name"
                              :key :name))
                   :handler #'delete-user-handler)
                  (clingon:make-command
                   :name "rename-channel"
                   :description "Rename a channel"
                   :options (list
                             (clingon:make-option
                              :string
                              :description "old channel name"
                              :short-name #\o
                              :long-name "old"
                              :key :old)
                             (clingon:make-option
                              :string
                              :description "new channel name"
                              :short-name #\n
                              :long-name "new"
                              :key :new))
                   :handler #'rename-channel-handler)
                  (clingon:make-command
                   :name "rename-user"
                   :description "Rename a user"
                   :options (list
                             (clingon:make-option
                              :string
                              :description "old user name"
                              :short-name #\o
                              :long-name "old"
                              :key :old)
                             (clingon:make-option
                              :string
                              :description "new user name"
                              :short-name #\n
                              :long-name "new"
                              :key :new))
                   :handler #'rename-user-handler)
                  (clingon:make-command
                   :name "search"
                   :description "Search for a specific message in the log"
                   :options (list
                             (clingon:make-option
                              :string
                              :description "query string"
                              :short-name #\q
                              :long-name "query"
                              :key :query))
                   :handler #'search-handler)
                  (clingon:make-command
                   :name "history"
                   :description "Offline visualization of the chat history"
                   :options (list
                             (clingon:make-option
                              :string
                              :description "filter by channel"
                              :short-name #\c
                              :long-name "channel"
                              :key :channel)
                             (clingon:make-option
                              :string
                              :description "filter by user"
                              :short-name #\u
                              :long-name "user"
                              :key :user)
                             (clingon:make-option
                              :integer
                              :description "limit number of messages"
                              :short-name #\l
                              :long-name "limit"
                              :key :limit))
                   :handler #'history-handler)
                  (clingon:make-command
                   :name "stats"
                   :description "Simple stats"
                   :handler #'stats-handler))))

(defun main (&optional (argv (uiop:command-line-arguments)))
  (let ((app (make-admin-command)))
    (clingon:run app argv)))
