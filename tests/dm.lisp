(in-package :lisp-chat/tests)

(define-test dm-tests
  :parent lisp-chat-tests
  :description "Unit and integration tests for 1:1 private DM conversations, retention, replay, and deletion")

(define-test dm-room-helpers-unit
  :parent dm-tests
  (is string= "dm:alice:bob" (server:make-dm-room "alice" "bob"))
  (is string= "dm:alice:bob" (server:make-dm-room "Bob" "@alice"))
  (true (server:dm-room-p "dm:alice:bob"))
  (false (server:dm-room-p "#general"))
  (false (server:dm-room-p "dm:onlyone"))
  (is equal '("alice" "bob") (server:dm-room-participants "dm:alice:bob"))
  (true (server:dm-participant-p "dm:alice:bob" "alice"))
  (true (server:dm-participant-p "dm:alice:bob" "BOB"))
  (false (server:dm-participant-p "dm:alice:bob" "charlie"))
  (is string= "bob" (server:dm-room-peer "dm:alice:bob" "alice"))
  (is string= "alice" (server:dm-room-peer "dm:alice:bob" "bob"))
  (is eq nil (server:dm-room-peer "dm:alice:bob" "charlie")))

(define-test dm-persistence-and-retention-pruning
  :parent dm-tests
  (let ((config:*dm-persistence-file* "dm-unit-msgs.test.sexp")
        (config:*dm-cursors-file* "dm-unit-cursors.test.sexp")
        (config:*dm-max-messages-per-room* 3)
        (config:*dm-retention-days* 30))
    (unwind-protect
         (progn
           (server:reset-dm-store)
           (when (probe-file config:*dm-persistence-file*)
             (delete-file config:*dm-persistence-file*))
           (when (probe-file config:*dm-cursors-file*)
             (delete-file config:*dm-cursors-file*))
           ;; Send 5 messages while recipient is offline -> retention cap keeps newest 3
           (dotimes (i 5)
             (server:send-dm "alice" "bob" (format nil "msg-~a" i)))
           (let ((msgs (server:get-dm-room-messages "dm:alice:bob")))
             (is = 3 (length msgs))
             (is string= "msg-4" (server:dm-message-content (first msgs)))
             (is string= "msg-2" (server:dm-message-content (third msgs))))
           ;; Reload from disk and confirm messages, cursors, and monotonic ID survive
           (server:reset-dm-store)
           (server:load-dm-store)
           (let ((reloaded (server:get-dm-room-messages "dm:alice:bob")))
             (is = 3 (length reloaded))
             (is = 5 (server:get-dm-cursor "alice" "dm:alice:bob"))
             (is = 0 (server:get-dm-cursor "bob" "dm:alice:bob")))
           ;; Next message gets ID > 5
           (let ((next-msg (server:send-dm "alice" "bob" "msg-5")))
             (true (> (server:dm-message-id next-msg) 5))))
      (server:reset-dm-store)
      (when (probe-file config:*dm-persistence-file*)
        (delete-file config:*dm-persistence-file*))
      (when (probe-file config:*dm-cursors-file*)
        (delete-file config:*dm-cursors-file*)))))

(define-test dm-broken-connection-and-partial-replay
  :parent dm-tests
  (let ((config:*dm-persistence-file* "dm-replay-msgs.test.sexp")
        (config:*dm-cursors-file* "dm-replay-cursors.test.sexp")
        (orig-send (symbol-function 'lisp-chat/server::send-message))
        (orig-delete (symbol-function 'lisp-chat/server::client-delete))
        (sent-to-bob nil)
        (deleted-clients nil)
        (fail-mode nil))
    (unwind-protect
         (progn
           (server:reset-dm-store)
           (let ((stale-bob (lisp-chat/server::make-client
                              :name "bob" :socket :stale :address "127.0.0.1"
                              :time (lisp-chat/server::get-time)
                              :active-channel "#general"))
                  (good-bob (lisp-chat/server::make-client
                             :name "bob" :socket :good :address "127.0.0.1"
                             :time (lisp-chat/server::get-time)
                             :active-channel "#general"))
                  (flaky-bob (lisp-chat/server::make-client
                              :name "bob" :socket :flaky :address "127.0.0.1"
                              :time (lisp-chat/server::get-time)
                              :active-channel "#general"))
                  (flaky-count 0))
             ;; Mock send-message & client-delete to simulate broken/flaky sockets
             (setf (symbol-function 'lisp-chat/server::send-message)
                   (lambda (client message)
                     (let ((sock (lisp-chat/server::client-socket client)))
                       (cond
                         ((eq sock :stale)
                          (error "Simulated broken pipe"))
                         ((eq sock :flaky)
                          (incf flaky-count)
                          (if (and fail-mode (= flaky-count 2))
                              (error "Simulated mid-replay disconnect")
                              (push message sent-to-bob)))
                         ((eq sock :good)
                          (push message sent-to-bob))))))
             (setf (symbol-function 'lisp-chat/server::client-delete)
                   (lambda (client)
                     (push (lisp-chat/server::client-socket client) deleted-clients)
                     (bt:with-lock-held (lisp-chat/server::*client-lock*)
                       (setf server:*clients* (remove client server:*clients*)))))
             ;; Register Bob's stale socket
             (bt:with-lock-held (lisp-chat/server::*client-lock*)
               (push stale-bob server:*clients*))
             ;; Alice sends 3 DMs while Bob's socket is broken
             (server:send-dm "alice" "bob" "first-secret")
             (server:send-dm "alice" "bob" "second-secret")
             (server:send-dm "alice" "bob" "third-secret")
             ;; Broken socket was deleted and Bob's cursor was NOT advanced
             (true (member :stale deleted-clients))
             (is = 0 (server:get-dm-cursor "bob" "dm:alice:bob"))
             ;; Bob reconnects on flaky socket that fails on message #2
             (setf fail-mode t)
             (server:replay-pending-dms flaky-bob)
             ;; Message #1 succeeded -> cursor advanced to 1; #2 failed -> flaky-bob deleted
             (is = 1 (server:get-dm-cursor "bob" "dm:alice:bob"))
             (true (member :flaky deleted-clients))
             (is = 1 (length sent-to-bob))
             ;; Bob reconnects on good socket -> receives #2 and #3 in chronological order
             (setf sent-to-bob nil)
             (server:replay-pending-dms good-bob)
             (is = 2 (length sent-to-bob))
             (is = 3 (server:get-dm-cursor "bob" "dm:alice:bob"))
             (true (search "second-secret" (second sent-to-bob)))
             (true (search "third-secret" (first sent-to-bob)))
             ;; Replaying again sends nothing (idempotent)
             (setf sent-to-bob nil)
             (server:replay-pending-dms good-bob)
             (is = 0 (length sent-to-bob))))
      (setf (symbol-function 'lisp-chat/server::send-message) orig-send
            (symbol-function 'lisp-chat/server::client-delete) orig-delete)
      (server:reset-dm-store)
      (when (probe-file config:*dm-persistence-file*)
        (delete-file config:*dm-persistence-file*))
      (when (probe-file config:*dm-cursors-file*)
        (delete-file config:*dm-cursors-file*)))))

(define-test dm-push-payload-uses-dm-url
  :parent dm-tests
  (let* ((payload-json (lisp-chat/server::build-push-payload
                        "alice" "secret ping" "dm:alice:bob"))
         (parsed (yason:parse payload-json)))
    (is string= "DM — @alice" (gethash "title" parsed))
    (is string= "secret ping" (gethash "body" parsed))
    (is string= "/?dm=alice" (gethash "url" parsed))))

(define-test dm-clear-prevents-second-user-query-and-isolation
  :parent dm-tests
  (let ((config:*dm-persistence-file* "dm-clear-msgs.test.sexp")
        (config:*dm-cursors-file* "dm-clear-cursors.test.sexp"))
    (unwind-protect
         (progn
           (server:reset-dm-store)
           (let ((alice (lisp-chat/server::make-client
                         :name "alice" :socket nil :address "127.0.0.1"
                         :time (lisp-chat/server::get-time)
                         :active-channel "#general"))
                 (bob (lisp-chat/server::make-client
                       :name "bob" :socket nil :address "127.0.0.1"
                       :time (lisp-chat/server::get-time)
                       :active-channel "#general"))
                 (charlie (lisp-chat/server::make-client
                           :name "charlie" :socket nil :address "127.0.0.1"
                           :time (lisp-chat/server::get-time)
                           :active-channel "#general")))
             ;; Alice sends multi-word DM while Bob is offline
             (lisp-chat/commands:call-command alice "/dm bob top secret message")
             (is = 1 (length (server:get-dm-room-messages "dm:alice:bob")))
             ;; Isolation: charlie cannot join dm:alice:bob
             (let ((deny (lisp-chat/commands:call-command charlie "/join dm:alice:bob")))
               (true (search "access denied" deny)))
             ;; Isolation: /channels :all t never lists dm:alice:bob
             (let ((chans (lisp-chat/commands:call-command charlie "/channels :all t")))
               (false (search "dm:alice:bob" chans)))
             ;; Bob enters DM conversation and can query history with /log before deletion
             (lisp-chat/commands:call-command bob "/dm alice")
             (is string= "dm:alice:bob" (server:client-active-channel bob))
             (let ((log-before (lisp-chat/commands:call-command bob "/log :depth 10")))
               (true (search "top secret message" log-before)))
             ;; Alice deletes the private conversation via /dm-clear
             (let ((clear-resp (lisp-chat/commands:call-command alice "/dm-clear bob")))
               (true (search "Deleted 1 message" clear-resp)))
             ;; Bob can NO LONGER query it via /log, /dms, or disk reload
             (let ((log-after (lisp-chat/commands:call-command bob "/log :depth 10")))
               (false (search "top secret message" log-after)))
             (let ((dms-after (lisp-chat/commands:call-command bob "/dms")))
               (true (search "dms: none" dms-after)))
             (server:reset-dm-store)
             (server:load-dm-store)
             (is eq nil (server:get-dm-room-messages "dm:alice:bob"))))
      (server:reset-dm-store)
      (when (probe-file config:*dm-persistence-file*)
        (delete-file config:*dm-persistence-file*))
      (when (probe-file config:*dm-cursors-file*)
        (delete-file config:*dm-cursors-file*)))))
