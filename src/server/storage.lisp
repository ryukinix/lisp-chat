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
