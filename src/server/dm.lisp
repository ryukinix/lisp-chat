(in-package :lisp-chat/server)

(defstruct dm-message
  (id 0 :type integer)
  (room "" :type string)
  (from "" :type string)
  (to "" :type string)
  (content "" :type string)
  (time nil))

(defvar *dm-lock* (bt:make-lock "dm-store-lock"))
(defvar *dm-messages* (make-hash-table :test 'equal)
  "Map of canonical DM room string (dm:a:b) -> list of DM-MESSAGE structs (newest-first).")
(defvar *dm-cursors* (make-hash-table :test 'equal)
  "Map of \"username|room\" -> integer last-delivered DM message ID.")
(defvar *dm-next-id* 1
  "Monotonic integer ID generator for DM messages.")

(defun dm-room-p (channel)
  "Return T if CHANNEL is a canonical 1:1 DM room identifier (dm:userA:userB)."
  (and (stringp channel)
       (startswith (string-downcase channel) "dm:")
       (= (count #\: channel) 2)))

(defun make-dm-room (user-a user-b)
  "Construct a canonical 1:1 DM room ID for USER-A and USER-B (sorted lowercase)."
  (let ((a (string-downcase (string-left-trim "@" (normalize-username user-a))))
        (b (string-downcase (string-left-trim "@" (normalize-username user-b)))))
    (if (string<= a b)
        (format nil "dm:~a:~a" a b)
        (format nil "dm:~a:~a" b a))))

(defun dm-room-participants (room)
  "Return (list user-a user-b) for a canonical DM ROOM, or NIL if invalid."
  (when (dm-room-p room)
    (let ((parts (uiop:split-string (string-downcase room) :separator ":")))
      (when (and (= (length parts) 3)
                 (plusp (length (second parts)))
                 (plusp (length (third parts))))
        (list (second parts) (third parts))))))

(defun dm-participant-p (room username)
  "Return T if USERNAME is one of the two participants in DM ROOM."
  (when (and (stringp username) (plusp (length username)))
    (let ((participants (dm-room-participants room)))
      (and participants
           (not (null (member username participants :test #'string-equal)))))))

(defun dm-room-peer (room self-name)
  "Return the other participant's username in DM ROOM given SELF-NAME, or NIL."
  (let ((participants (dm-room-participants room)))
    (when (and participants (member self-name participants :test #'string-equal))
      (if (string-equal (first participants) self-name)
          (second participants)
          (first participants)))))

(defun dm-cursor-key (username room)
  "Build the hash key for *dm-cursors*."
  (format nil "~a|~a" (string-downcase username) (string-downcase room)))

(defun get-dm-cursor (username room)
  "Return the last delivered DM message ID for USERNAME in ROOM (0 if none)."
  (bt:with-lock-held (*dm-lock*)
    (or (gethash (dm-cursor-key username room) *dm-cursors*) 0)))

(defun save-dm-cursors-unlocked ()
  "Atomically persist *dm-cursors* to config:*dm-cursors-file* (caller holds *dm-lock*)."
  (let ((alist nil))
    (maphash (lambda (k v)
               (when (and (stringp k) (integerp v))
                 (push (cons k v) alist)))
             *dm-cursors*)
    (write-sexp-atomically config:*dm-cursors-file* alist)))

(defun set-dm-cursor (username room msg-id)
  "Advance USERNAME's delivery cursor in ROOM to at least MSG-ID and persist."
  (when (and (stringp username) (stringp room) (integerp msg-id))
    (bt:with-lock-held (*dm-lock*)
      (let* ((key (dm-cursor-key username room))
             (current (or (gethash key *dm-cursors*) 0)))
        (when (> msg-id current)
          (setf (gethash key *dm-cursors*) msg-id)
          (save-dm-cursors-unlocked))))))

(defun dm-message-expired-p (msg now-universal)
  "Return T if MSG is older than config:*dm-retention-days*."
  (let ((days config:*dm-retention-days*))
    (and (numberp days)
         (plusp days)
         (let ((msg-time (dm-message-time msg)))
           (and msg-time
                (> (- now-universal (message-universal-time
                                     (make-message :time msg-time)))
                   (* days 86400)))))))

(defun prune-dm-messages-list (msgs &optional (now-universal (get-universal-time)))
  "Drop expired messages and cap newest-first MSGS list to config:*dm-max-messages-per-room*."
  (let ((fresh (remove-if (lambda (m) (dm-message-expired-p m now-universal)) msgs))
        (cap config:*dm-max-messages-per-room*))
    (if (and (integerp cap) (plusp cap) (> (length fresh) cap))
        (subseq fresh 0 cap)
        fresh)))

(defun save-dm-messages-unlocked ()
  "Atomically persist *dm-messages* to config:*dm-persistence-file* (caller holds *dm-lock*)."
  (let ((alist nil))
    (maphash (lambda (room msgs)
               (when msgs
                 (push (cons room msgs) alist)))
             *dm-messages*)
    (write-sexp-atomically config:*dm-persistence-file* alist)))

(defun reset-dm-store ()
  "Clear in-memory DM messages, cursors, and reset *dm-next-id*."
  (bt:with-lock-held (*dm-lock*)
    (clrhash *dm-messages*)
    (clrhash *dm-cursors*)
    (setf *dm-next-id* 1)))

(defun load-dm-messages ()
  "Load *dm-messages* from config:*dm-persistence-file*, pruning expired/excess entries."
  (when (probe-file config:*dm-persistence-file*)
    (with-open-file (in config:*dm-persistence-file* :direction :input)
      (handler-case
          (let ((alist (read in nil nil))
                (now (get-universal-time))
                (max-id 0)
                (pruned-any nil))
            (when (listp alist)
              (bt:with-lock-held (*dm-lock*)
                (clrhash *dm-messages*)
                (dolist (pair alist)
                  (let* ((room (car pair))
                         (raw-msgs (cdr pair))
                         (kept (prune-dm-messages-list raw-msgs now)))
                    (when (/= (length kept) (length raw-msgs))
                      (setf pruned-any t))
                    (dolist (m kept)
                      (when (> (dm-message-id m) max-id)
                        (setf max-id (dm-message-id m))))
                    (when kept
                      (setf (gethash room *dm-messages*) kept))))
                (setf *dm-next-id* (max *dm-next-id* (1+ max-id)))
                (when pruned-any
                  (save-dm-messages-unlocked)))
              (debug-format t "[info] dm rooms loaded: ~a~%" (hash-table-count *dm-messages*))))
        (error (e) (debug-format t "Error loading dm messages: ~a~%" e))))))

(defun load-dm-cursors ()
  "Load *dm-cursors* from config:*dm-cursors-file*."
  (when (probe-file config:*dm-cursors-file*)
    (with-open-file (in config:*dm-cursors-file* :direction :input)
      (handler-case
          (let ((alist (read in nil nil)))
            (when (listp alist)
              (bt:with-lock-held (*dm-lock*)
                (clrhash *dm-cursors*)
                (dolist (pair alist)
                  (when (and (stringp (car pair)) (integerp (cdr pair)))
                    (setf (gethash (car pair) *dm-cursors*) (cdr pair)))))
              (debug-format t "[info] dm cursors loaded: ~a entries~%" (length alist))))
        (error (e) (debug-format t "Error loading dm cursors: ~a~%" e))))))

(defun load-dm-store ()
  "Load both DM messages and DM delivery cursors from disk."
  (load-dm-messages)
  (load-dm-cursors))

(defun dm-message->message (dm)
  "Convert a DM-MESSAGE struct into a standard MESSAGE struct for formatting/log/search."
  (make-message :from (dm-message-from dm)
                :content (dm-message-content dm)
                :time (dm-message-time dm)
                :channel (dm-message-room dm)))

(defun get-dm-room-messages (room)
  "Return a shallow copy of newest-first DM-MESSAGE structs in ROOM."
  (bt:with-lock-held (*dm-lock*)
    (copy-list (gethash (string-downcase room) *dm-messages*))))

(defun get-dm-room-messages-as-messages (room)
  "Return newest-first MESSAGE structs for ROOM (compatible with /log and /search)."
  (mapcar #'dm-message->message (get-dm-room-messages room)))

(defun get-clients-by-name (username)
  "Return all active CLIENT instances matching USERNAME (case-insensitive)."
  (bt:with-lock-held (*client-lock*)
    (remove-if-not (lambda (c) (string-equal (client-name c) username))
                   *clients*)))

(defun format-dm-for-client (dm client)
  "Format DM for CLIENT: standard message if viewing that DM room, or dm:<from> tag otherwise."
  (if (string-equal (client-active-channel client) (dm-message-room dm))
      (formatted-message (dm-message->message dm) :client client)
      (let* ((from-tag (if (string-equal (client-name client) (dm-message-from dm))
                           (format nil "dm->~a" (dm-message-to dm))
                           (format nil "dm:~a" (dm-message-from dm))))
             (msg (make-message :from from-tag
                                :content (dm-message-content dm)
                                :time (dm-message-time dm)
                                :channel (dm-message-room dm))))
        (formatted-message msg :client client))))

(defun deliver-dm-to-user-clients (dm username)
  "Attempt to deliver DM to all online sockets of USERNAME.
   Deletes broken sockets on error and returns T if at least one socket succeeded."
  (let ((targets (get-clients-by-name username))
        (delivered-any nil))
    (dolist (target targets)
      (handler-case
          (progn
            (send-message target (format-dm-for-client dm target))
            (setf delivered-any t))
        (error (e)
          (debug-format t "[warn] DM delivery failed for ~a: ~a~%" username e)
          (client-delete target))))
    delivered-any))

(defun notify-dm-push (dm)
  "Enqueue Web Push notifications for all registered sessions of DM recipient."
  (let ((recipient (dm-message-to dm))
        (sessions nil))
    (bt:with-lock-held (*username-to-sessions-lock*)
      (maphash (lambda (k v)
                 (when (string-equal k recipient)
                   (setf sessions (append sessions v))))
               *username-to-sessions*))
    (dolist (sid (remove-duplicates sessions :test #'string-equal))
      (push-notification sid
                         (dm-message-from dm)
                         (dm-message-content dm)
                         :channel (dm-message-room dm)))))

(defun send-dm (from-name to-name content)
  "Record a 1:1 DM from FROM-NAME to TO-NAME, persist with retention, deliver to online
   sockets, advance cursors only on confirmed send, and trigger Web Push when undelivered.
   Returns the created DM-MESSAGE."
  (let* ((from (normalize-username from-name))
         (to (normalize-username to-name))
         (room (make-dm-room from to))
         (dm nil))
    (bt:with-lock-held (*dm-lock*)
      (let ((id *dm-next-id*))
        (incf *dm-next-id*)
        (setf dm (make-dm-message :id id
                                  :room room
                                  :from from
                                  :to to
                                  :content content
                                  :time (get-time)))
        (let ((updated (prune-dm-messages-list
                        (cons dm (gethash room *dm-messages*)))))
          (setf (gethash room *dm-messages*) updated)
          (save-dm-messages-unlocked))))
    ;; Echo to all active sender sockets and advance sender cursor
    (deliver-dm-to-user-clients dm from)
    (set-dm-cursor from room (dm-message-id dm))
    ;; Deliver to recipient sockets; only advance cursor if at least one socket succeeded
    (let ((recipient-delivered (deliver-dm-to-user-clients dm to)))
      (if recipient-delivered
          (set-dm-cursor to room (dm-message-id dm))
          (notify-dm-push dm)))
    dm))

(defun replay-pending-dms (client)
  "Replay all undelivered DM messages for CLIENT in chronological order.
   Advances the delivery cursor per message only after SEND-MESSAGE succeeds;
   stops and deletes CLIENT if the socket fails mid-replay."
  (let ((username (client-name client))
        (pending nil))
    (bt:with-lock-held (*dm-lock*)
      (maphash (lambda (room msgs)
                 (when (dm-participant-p room username)
                   (let ((cursor (or (gethash (dm-cursor-key username room) *dm-cursors*) 0)))
                     (dolist (m msgs)
                       (when (> (dm-message-id m) cursor)
                         (push m pending))))))
               *dm-messages*))
    (setf pending (sort pending #'< :key #'dm-message-id))
    (dolist (dm pending)
      (handler-case
          (progn
            (send-message client (format-dm-for-client dm client))
            (set-dm-cursor username (dm-message-room dm) (dm-message-id dm)))
        (error (e)
          (debug-format t "[warn] DM replay interrupted for ~a at id ~a: ~a~%"
                        username (dm-message-id dm) e)
          (client-delete client)
          (return))))
    (length pending)))

(defun list-user-dm-rooms (username)
  "Return a list of (room peer total-count unread-count last-id) for USERNAME,
   sorted by last-id descending."
  (let ((entries nil))
    (bt:with-lock-held (*dm-lock*)
      (maphash (lambda (room msgs)
                 (when (and msgs (dm-participant-p room username))
                   (let* ((peer (dm-room-peer room username))
                          (cursor (or (gethash (dm-cursor-key username room) *dm-cursors*) 0))
                          (unread (count-if (lambda (m) (> (dm-message-id m) cursor)) msgs))
                          (last-id (dm-message-id (first msgs))))
                     (push (list room peer (length msgs) unread last-id) entries))))
               *dm-messages*))
    (sort entries #'> :key #'fifth)))

(defun clear-dm-room (requester-name peer-name)
  "Permanently delete all retained DM messages and cursors for the 1:1 conversation
   between REQUESTER-NAME and PEER-NAME so neither user can query or replay them.
   Returns the number of deleted messages."
  (let* ((req (normalize-username requester-name))
         (peer (normalize-username peer-name))
         (room (make-dm-room req peer))
         (deleted-count 0))
    (bt:with-lock-held (*dm-lock*)
      (let ((existing (gethash room *dm-messages*)))
        (setf deleted-count (length existing))
        (remhash room *dm-messages*)
        (remhash (dm-cursor-key req room) *dm-cursors*)
        (remhash (dm-cursor-key peer room) *dm-cursors*)
        (save-dm-messages-unlocked)
        (save-dm-cursors-unlocked)))
    ;; Ephemeral notice to online peer sockets (never stored in *dm-messages*)
    (dolist (target (get-clients-by-name peer))
      (handler-case
          (send-message target
                        (command-message
                         (format nil "Private conversation history with @~a was deleted by @~a."
                                 req req)
                         :client target))
        (error () (client-delete target))))
    deleted-count))
