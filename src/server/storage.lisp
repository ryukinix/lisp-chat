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

(defun write-sexp-atomically (path data)
  "Write DATA readably to PATH via a temporary file, fsync, and atomic rename."
  (let ((tmp-path (format nil "~a.tmp" (namestring path)))
        (renamed nil))
    (unwind-protect
         (progn
           (with-open-file (out tmp-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
             (let ((*print-readably* t))
               (print data out)
               (finish-output out))
             #+sbcl
             (let ((fsync-fn (find-symbol "FSYNC" :sb-posix)))
               (when (and fsync-fn (fboundp fsync-fn))
                 (handler-case
                     (funcall fsync-fn (sb-sys:fd-stream-fd out))
                   (error () nil)))))
           (rename-file tmp-path path)
           (setf renamed t))
      (when (and (not renamed) (probe-file tmp-path))
        (handler-case
            (delete-file tmp-path)
          (file-error () nil))))))

(defun load-or-create-vapid-keys ()
  "Initialize VAPID keys from environment variables, config:*vapid-keys-file*,
   or generate and persist a new keypair if neither exists."
  (let ((pub (uiop:getenv "VAPID_PUBLIC_KEY"))
        (priv (uiop:getenv "VAPID_PRIVATE_KEY"))
        (subj (uiop:getenv "VAPID_SUBJECT"))
        (vapid-file config:*vapid-keys-file*))
    (when (and subj (plusp (length subj)))
      (setq *vapid-subject* subj))
    (cond
      ((and pub priv (plusp (length pub)) (plusp (length priv)))
       (setq *vapid-public-key* pub)
       (setq *vapid-private-key* priv))
      ((probe-file vapid-file)
       (handler-case
           (with-open-file (in vapid-file :direction :input)
             (let ((keys (read in nil nil)))
               (if (and (listp keys) (stringp (first keys)) (stringp (second keys)))
                   (progn
                     (setq *vapid-public-key* (first keys))
                     (setq *vapid-private-key* (second keys)))
                   (multiple-value-bind (new-pub new-priv) (cl-web-push:generate-vapid-keys)
                     (setq *vapid-public-key* new-pub)
                     (setq *vapid-private-key* new-priv)
                     (write-sexp-atomically vapid-file (list new-pub new-priv))))))
         (error (e)
           (debug-format t "Error loading VAPID keys: ~a~%" e)
           (multiple-value-bind (new-pub new-priv) (cl-web-push:generate-vapid-keys)
             (setq *vapid-public-key* new-pub)
             (setq *vapid-private-key* new-priv)
             (write-sexp-atomically vapid-file (list new-pub new-priv))))))
      (t
       (multiple-value-bind (new-pub new-priv) (cl-web-push:generate-vapid-keys)
         (setq *vapid-public-key* new-pub)
         (setq *vapid-private-key* new-priv)
         (write-sexp-atomically vapid-file (list new-pub new-priv)))))))

(defun save-push-subscriptions-unlocked ()
  "Write *push-subscriptions* hash table to disk atomically (caller must hold *push-subscriptions-lock*)."
  (let ((alist nil))
    (maphash (lambda (k v)
               (when v
                 (push (cons k v) alist)))
             *push-subscriptions*)
    (write-sexp-atomically config:*push-subscriptions-file* alist)))

(defun save-push-subscriptions ()
  "Write *push-subscriptions* hash table to disk as an alist."
  (bt:with-lock-held (*push-subscriptions-lock*)
    (save-push-subscriptions-unlocked)))

(defun load-push-subscriptions ()
  "Load *push-subscriptions* from disk."
  (when (probe-file config:*push-subscriptions-file*)
    (with-open-file (in config:*push-subscriptions-file* :direction :input)
      (handler-case
          (let ((alist (read in nil nil)))
            (when alist
              (bt:with-lock-held (*push-subscriptions-lock*)
                (dolist (pair alist)
                  (setf (gethash (car pair) *push-subscriptions*) (cdr pair))))
              (debug-format t "[info] push subscriptions loaded: ~a entries~%" (length alist))))
        (error (e) (debug-format t "Error loading push subscriptions: ~a~%" e))))))

(defun extract-subscription-endpoint (sub-json)
  "Extract the endpoint URL from a push subscription JSON string, or NIL on error."
  (handler-case
      (let ((parsed (yason:parse sub-json)))
        (when (hash-table-p parsed)
          (gethash "endpoint" parsed)))
    (error () nil)))

(defun subscribe-push-session (session-id sub-json)
  "Register SUB-JSON under SESSION-ID, deduplicating by endpoint URL and persisting to disk."
  (let ((new-endpoint (extract-subscription-endpoint sub-json)))
    (bt:with-lock-held (*push-subscriptions-lock*)
      (let* ((existing (gethash session-id *push-subscriptions*))
             (filtered (if new-endpoint
                           (remove-if (lambda (s)
                                        (let ((ep (extract-subscription-endpoint s)))
                                          (and ep (string= ep new-endpoint))))
                                      existing)
                           (remove sub-json existing :test #'string-equal))))
        (setf (gethash session-id *push-subscriptions*)
              (cons sub-json filtered))
        (save-push-subscriptions-unlocked)))))

(defun unsubscribe-push-session (session-id &optional endpoint)
  "Remove push subscriptions for SESSION-ID (only matching ENDPOINT if provided) and persist."
  (bt:with-lock-held (*push-subscriptions-lock*)
    (if endpoint
        (let* ((existing (gethash session-id *push-subscriptions*))
               (remaining (remove-if (lambda (s)
                                       (let ((ep (extract-subscription-endpoint s)))
                                         (or (and ep (string= ep endpoint))
                                             (string-equal s endpoint))))
                                     existing)))
          (if remaining
              (setf (gethash session-id *push-subscriptions*) remaining)
              (remhash session-id *push-subscriptions*)))
        (remhash session-id *push-subscriptions*))
    (save-push-subscriptions-unlocked)))

(defun save-user-sessions-unlocked ()
  "Write *username-to-sessions* hash table to disk atomically (caller must hold *username-to-sessions-lock*)."
  (let ((alist nil))
    (maphash (lambda (k v)
               (when v
                 (push (cons k v) alist)))
             *username-to-sessions*)
    (write-sexp-atomically config:*user-sessions-file* alist)))

(defun save-user-sessions ()
  "Write *username-to-sessions* hash table to disk as an alist."
  (bt:with-lock-held (*username-to-sessions-lock*)
    (save-user-sessions-unlocked)))

(defun load-user-sessions ()
  "Load *username-to-sessions* from disk."
  (when (probe-file config:*user-sessions-file*)
    (with-open-file (in config:*user-sessions-file* :direction :input)
      (handler-case
          (let ((alist (read in nil nil)))
            (when alist
              (bt:with-lock-held (*username-to-sessions-lock*)
                (dolist (pair alist)
                  (setf (gethash (car pair) *username-to-sessions*) (cdr pair))))
              (debug-format t "[info] user sessions loaded: ~a entries~%" (length alist))))
        (error (e) (debug-format t "Error loading user sessions: ~a~%" e))))))

(defun register-user-session (username session-id)
  "Associate SESSION-ID with USERNAME, removing SESSION-ID from any previous username
   so /nick changes and re-logins stay consistent, while preserving other devices/browsers."
  (when (and (stringp username) (plusp (length username))
             (stringp session-id) (plusp (length session-id)))
    (bt:with-lock-held (*username-to-sessions-lock*)
      (let ((empty-keys nil))
        (maphash (lambda (user sids)
                   (unless (string-equal user username)
                     (let ((cleaned (remove session-id sids :test #'string-equal)))
                       (if cleaned
                           (setf (gethash user *username-to-sessions*) cleaned)
                           (push user empty-keys)))))
                 *username-to-sessions*)
        (dolist (k empty-keys)
          (remhash k *username-to-sessions*)))
      (let ((sessions (gethash username *username-to-sessions*)))
        (unless (find session-id sessions :test #'string-equal)
          (setf (gethash username *username-to-sessions*)
                (cons session-id sessions))))
      (save-user-sessions-unlocked))))
