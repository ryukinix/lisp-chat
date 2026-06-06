(in-package :lisp-chat/server)

(defvar *persistence-queue* '())
(defvar *persistence-lock* (bt:make-lock))
(defvar *persistence-semaphore* (bt:make-semaphore :name "persistence-semaphore"))

(defun save-message-to-disk (message-raw)
  "Save a single MESSAGE-RAW struct into config:*persistence-file*"
  (with-open-file (out config:*persistence-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (let ((*print-readably* t))
      (print message-raw out)
      (finish-output out))))

(defun persistence-worker ()
  "Dedicated worker thread to save messages to disk asynchronously, preventing broadcast loop blocking."
  (loop when (bt:wait-on-semaphore *persistence-semaphore*)
        do (let ((message-to-save
                   (bt:with-lock-held (*persistence-lock*)
                     (pop *persistence-queue*))))
             (when message-to-save
               (save-message-to-disk message-to-save)))))

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

(defun save-push-subscriptions ()
  "Write *push-subscriptions* hash table to disk as an alist."
  (with-open-file (out config:*push-subscriptions-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-readably* t))
      (let ((alist nil))
        (maphash (lambda (k v) (push (cons k v) alist)) *push-subscriptions*)
        (print alist out))
      (finish-output out))))

(defun load-push-subscriptions ()
  "Load *push-subscriptions* from disk."
  (when (probe-file config:*push-subscriptions-file*)
    (with-open-file (in config:*push-subscriptions-file* :direction :input)
      (handler-case
          (let ((alist (read in nil nil)))
            (when alist
              (dolist (pair alist)
                (setf (gethash (car pair) *push-subscriptions*) (cdr pair)))
              (debug-format t "[info] push subscriptions loaded: ~a entries~%" (length alist))))
        (error (e) (debug-format t "Error loading push subscriptions: ~a~%" e))))))

(defun save-user-sessions ()
  "Write *username-to-sessions* hash table to disk as an alist."
  (with-open-file (out config:*user-sessions-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*print-readably* t))
      (let ((alist nil))
        (maphash (lambda (k v) (push (cons k v) alist)) *username-to-sessions*)
        (print alist out))
      (finish-output out))))

(defun load-user-sessions ()
  "Load *username-to-sessions* from disk."
  (when (probe-file config:*user-sessions-file*)
    (with-open-file (in config:*user-sessions-file* :direction :input)
      (handler-case
          (let ((alist (read in nil nil)))
            (when alist
              (dolist (pair alist)
                (setf (gethash (car pair) *username-to-sessions*) (cdr pair)))
              (debug-format t "[info] user sessions loaded: ~a entries~%" (length alist))))
        (error (e) (debug-format t "Error loading user sessions: ~a~%" e))))))
