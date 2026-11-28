(in-package :lisp-chat/tests)

;;; Tests for push-notification branch features:
;;;   - session-id parsed from WebSocket query string and applied to client
;;;   - username→session-id mapping registered on login
;;;   - subscribe-push API keyed by session_id
;;;   - push-subscriptions saved to disk and loaded back on restart
;;;   - stale subscription cleanup (410/404) updates the persisted file
;;;   - parse-session-id helper

(define-test push-notification-tests
  :parent lisp-chat-tests)

;;; --- Unit tests ---

(define-test parse-session-id-valid
  :parent push-notification-tests
  (is string= "abc-123"
      (server::parse-session-id "tz=-3&session_id=abc-123&expand_reply=false"))
  (is string= "abc-123"
      (server::parse-session-id "session_id=abc-123"))
  (is string= "abc-123"
      (server::parse-session-id "session_id=abc-123&channel=%23general")))

(define-test parse-session-id-missing
  :parent push-notification-tests
  (is eq nil (server::parse-session-id "tz=-3&channel=general"))
  (is eq nil (server::parse-session-id ""))
  (is eq nil (server::parse-session-id nil)))

(define-test push-subscriptions-save-and-load
  :parent push-notification-tests
  (server:reset-server)
  (let ((test-file "push-subscriptions.unit-test.sexp")
        (config:*push-subscriptions-file* "push-subscriptions.unit-test.sexp"))
    (unwind-protect
         (progn
           ;; Insert two subscriptions under different session-ids
           (setf (gethash "sid-aaa" server:*push-subscriptions*)
                 '("{\"endpoint\":\"https://example.com/push/1\"}"))
           (setf (gethash "sid-bbb" server:*push-subscriptions*)
                 '("{\"endpoint\":\"https://example.com/push/2\"}"
                   "{\"endpoint\":\"https://example.com/push/3\"}"))
           (server::save-push-subscriptions)
           ;; Clear and reload
           (setf server:*push-subscriptions* (make-hash-table :test 'equal))
           (server::load-push-subscriptions)
           ;; Verify both entries round-tripped
           (let ((a (gethash "sid-aaa" server:*push-subscriptions*))
                 (b (gethash "sid-bbb" server:*push-subscriptions*)))
             (true a)
             (is = 1 (length a))
             (true (search "push/1" (first a)))
             (true b)
             (is = 2 (length b))))
      (when (probe-file test-file) (delete-file test-file))
      (server:reset-server))))

(define-test push-subscriptions-loaded-at-startup
  :parent push-notification-tests
  ;; After reset-server, *push-subscriptions* should be a fresh empty table
  (server:reset-server)
  (is = 0 (hash-table-count server:*push-subscriptions*)))

(define-test stale-subscription-cleanup
  :parent push-notification-tests
  (server:reset-server)
  (let ((config:*push-subscriptions-file* "push-subscriptions.cleanup-test.sexp"))
    (unwind-protect
         (progn
           (setf (gethash "sid-cleanup" server:*push-subscriptions*)
                 '("{\"endpoint\":\"https://example.com/gone\"}"
                   "{\"endpoint\":\"https://example.com/ok\"}"))
           ;; Simulate 410-Gone removal
           (bt:with-lock-held (server::*push-subscriptions-lock*)
             (setf (gethash "sid-cleanup" server:*push-subscriptions*)
                   (remove "{\"endpoint\":\"https://example.com/gone\"}"
                           (gethash "sid-cleanup" server:*push-subscriptions*)
                           :test #'string-equal)))
           (server::save-push-subscriptions)
           ;; Reload and verify only the live subscription survives
           (setf server:*push-subscriptions* (make-hash-table :test 'equal))
           (server::load-push-subscriptions)
           (let ((subs (gethash "sid-cleanup" server:*push-subscriptions*)))
             (is = 1 (length subs))
             (true (search "ok" (first subs)))))
      (let ((f "push-subscriptions.cleanup-test.sexp"))
        (when (probe-file f) (delete-file f)))
      (server:reset-server))))

;;; --- Integration tests ---

(define-test websocket-session-id-from-query-string
  :parent push-notification-tests
  (server:reset-server)
  (let ((test-sid "test-sid-1234-abcd"))
    (with-websocket-client (client messages
                            :query-params (format nil "session_id=~a" test-sid))
      (ws-interaction client (lambda () messages)
        '(:expect "Type your username")
        "tester-sid"
        '(:expect "The user @tester-sid joined to the party!")
        '(:sleep 0.2))
      (let ((active-client (server:get-client "tester-sid")))
        (true active-client)
        ;; The client struct should carry the session-id from the URL
        (is string= test-sid (server:client-session-id active-client))))))

(define-test websocket-session-id-fallback-when-absent
  :parent push-notification-tests
  (server:reset-server)
  (with-websocket-client (client messages)
    (ws-interaction client (lambda () messages)
      '(:expect "Type your username")
      "tester-nosid"
      '(:expect "The user @tester-nosid joined to the party!")
      '(:sleep 0.2))
    (let ((active-client (server:get-client "tester-nosid")))
      (true active-client)
      ;; Should have a non-empty server-generated UUID
      (let ((sid (server:client-session-id active-client)))
        (true sid)
        (true (> (length sid) 0))
        (let ((sessions (gethash "tester-nosid" server:*username-to-sessions*)))
          (true sessions)
          (true (find sid sessions :test #'string-equal)))))))

(define-test subscribe-push-api-with-session-id
  :parent push-notification-tests
  (server:reset-server)
  (let ((test-sid "api-test-session-xyz")
        (fake-sub "{\"endpoint\":\"https://fcm.example.com/push/abc\",\"keys\":{\"p256dh\":\"x\",\"auth\":\"y\"}}"))
    ;; POST to subscribe-push with session_id param
    (multiple-value-bind (body status)
        (dex:post (api-url (format nil "/api/notifications/subscribe-push?session_id=~a" test-sid))
                  :content fake-sub
                  :headers '(("Content-Type" . "application/json")))
      (is = 200 status)
      (let ((data (yason:parse body)))
        (true (gethash "success" data))))
    ;; Check the subscription was registered under the session-id
    (let ((subs (gethash test-sid server:*push-subscriptions*)))
      (true subs)
      (is = 1 (length subs))
      (true (search "fcm.example.com" (first subs))))
    ;; Duplicate subscription must not be added
    (dex:post (api-url (format nil "/api/notifications/subscribe-push?session_id=~a" test-sid))
              :content fake-sub
              :headers '(("Content-Type" . "application/json")))
    (is = 1 (length (gethash test-sid server:*push-subscriptions*)))))

(define-test subscribe-push-api-missing-session-id
  :parent push-notification-tests
  (handler-case
      (dex:post (api-url "/api/notifications/subscribe-push")
                :content "{\"endpoint\":\"https://example.com/push\"}"
                :headers '(("Content-Type" . "application/json")))
    (dex:http-request-failed (e)
      (is = 400 (dex:response-status e))
      (let ((data (yason:parse (dex:response-body e))))
        (true (gethash "error" data))))))

(define-test subscribe-push-api-persists-to-disk
  :parent push-notification-tests
  (server:reset-server)
  (let ((test-sid "persist-test-session")
        (fake-sub "{\"endpoint\":\"https://push.example.com/persist\"}"))
    (dex:post (api-url (format nil "/api/notifications/subscribe-push?session_id=~a" test-sid))
              :content fake-sub
              :headers '(("Content-Type" . "application/json")))
    ;; The file should have been written
    (true (probe-file config:*push-subscriptions-file*))
    ;; Load into a fresh table to verify the file round-trips
    (let ((fresh (make-hash-table :test 'equal)))
      (with-open-file (in config:*push-subscriptions-file* :direction :input)
        (let ((alist (read in nil nil)))
          (dolist (pair alist)
            (setf (gethash (car pair) fresh) (cdr pair)))))
      (let ((subs (gethash test-sid fresh)))
        (true subs)
        (true (search "persist" (first subs)))))))

(define-test extract-mentions-unit
  :parent push-notification-tests
  (is equal '("alice" "bob")
      (server:extract-mentions "Hello @alice, check with @bob! Also @alice."))
  (is equal nil
      (server:extract-mentions "No mentions here or bare @ sign")))

(define-test extract-reply-targets-and-notification-targets
  :parent push-notification-tests
  (is equal '("alice")
      (server:extract-reply-targets "<#general: 2026-11-26 02:10:00 [alice]> Sounds great!"))
  (is equal '("alice" "bob")
      (server:extract-reply-targets
       "<#general: 2026-11-26 02:10:00 [alice]> <#dev: 2026-11-26 02:11:00 [bob]> both!"))
  ;; Mention + reply to same user deduplicates to a single target
  (is equal '("alice" "bob")
      (server:extract-notification-targets
       "<#general: 2026-11-26 02:10:00 [bob]> Hey @alice and @bob!"))
  ;; Human-readable push body replaces raw reply reference tag
  (let* ((payload-json (lisp-chat/server::build-push-payload
                        "charlie"
                        "<#general: 2026-11-26 02:10:00 [alice]> Sounds great!"
                        "#general"))
         (parsed (yason:parse payload-json)))
    (is string= "#general — @charlie" (gethash "title" parsed))
    (is string= "«reply to @alice» Sounds great!" (gethash "body" parsed))
    (is string= "#general" (gethash "channel" parsed))
    (is string= "/?general" (gethash "url" parsed)))
  ;; Channel with special characters is URL-encoded in notification link
  (let* ((payload-json (lisp-chat/server::build-push-payload
                        "charlie"
                        "Hey @alice!"
                        "#dev-team"))
         (parsed (yason:parse payload-json)))
    (is string= "/?dev-team" (gethash "url" parsed))))

(define-test nick-change-updates-user-sessions
  :parent push-notification-tests
  (server:reset-server)
  (let ((sid-chrome "sid-chrome-001")
        (sid-firefox "sid-firefox-002"))
    ;; Same user on two browsers
    (server:register-user-session "oldnick" sid-chrome)
    (server:register-user-session "oldnick" sid-firefox)
    (is = 2 (length (gethash "oldnick" server:*username-to-sessions*)))
    ;; Chrome browser changes nick to newnick
    (server:register-user-session "newnick" sid-chrome)
    ;; oldnick should still have firefox, not chrome
    (let ((old-sids (gethash "oldnick" server:*username-to-sessions*))
          (new-sids (gethash "newnick" server:*username-to-sessions*)))
      (is = 1 (length old-sids))
      (true (find sid-firefox old-sids :test #'string=))
      (false (find sid-chrome old-sids :test #'string=))
      (is = 1 (length new-sids))
      (true (find sid-chrome new-sids :test #'string=)))
    ;; Firefox also changes nick -> oldnick key should be cleaned up
    (server:register-user-session "newnick" sid-firefox)
    (is eq nil (gethash "oldnick" server:*username-to-sessions*))
    (is = 2 (length (gethash "newnick" server:*username-to-sessions*)))))

(define-test websocket-nick-command-updates-session-mapping
  :parent push-notification-tests
  (server:reset-server)
  (let ((test-sid "nick-ws-session-001"))
    (with-websocket-client (client messages
                            :query-params (format nil "session_id=~a" test-sid))
      (ws-interaction client (lambda () messages)
        '(:expect "Type your username")
        "alphauser"
        '(:expect "The user @alphauser joined to the party!")
        "/nick betauser"
        '(:expect "Your new nick is: @betauser")
        '(:sleep 0.2))
      (is eq nil (gethash "alphauser" server:*username-to-sessions*))
      (let ((beta-sids (gethash "betauser" server:*username-to-sessions*)))
        (true beta-sids)
        (true (find test-sid beta-sids :test #'string=))))))

(define-test subscribe-push-deduplicates-by-endpoint-and-unsubscribes
  :parent push-notification-tests
  (server:reset-server)
  (let ((test-sid "dedup-session-001")
        (sub-v1 "{\"endpoint\":\"https://push.example.com/dev1\",\"keys\":{\"p256dh\":\"old\",\"auth\":\"a\"}}")
        (sub-v2 "{\"endpoint\":\"https://push.example.com/dev1\",\"keys\":{\"p256dh\":\"new\",\"auth\":\"b\"}}"))
    ;; Subscribe v1 then v2 for the same endpoint -> should replace, keeping 1 entry
    (dex:post (api-url (format nil "/api/notifications/subscribe-push?session_id=~a" test-sid))
              :content sub-v1
              :headers '(("Content-Type" . "application/json")))
    (dex:post (api-url (format nil "/api/notifications/subscribe-push?session_id=~a" test-sid))
              :content sub-v2
              :headers '(("Content-Type" . "application/json")))
    (let ((subs (gethash test-sid server:*push-subscriptions*)))
      (is = 1 (length subs))
      (true (search "\"new\"" (first subs))))
    ;; Unsubscribe by endpoint
    (multiple-value-bind (body status)
        (dex:post (api-url (format nil "/api/notifications/unsubscribe-push?session_id=~a" test-sid))
                  :content "{\"endpoint\":\"https://push.example.com/dev1\"}"
                  :headers '(("Content-Type" . "application/json")))
      (is = 200 status)
      (true (gethash "success" (yason:parse body))))
    (is eq nil (gethash test-sid server:*push-subscriptions*))))

(define-test vapid-keys-persist-across-reloads
  :parent push-notification-tests
  (let ((config:*vapid-keys-file* "vapid.unit-test.sexp")
        (saved-pub server:*vapid-public-key*)
        (saved-priv server:*vapid-private-key*))
    (unwind-protect
         (progn
           (when (probe-file config:*vapid-keys-file*)
             (delete-file config:*vapid-keys-file*))
           (server:load-or-create-vapid-keys)
           (let ((pub1 server:*vapid-public-key*)
                 (priv1 server:*vapid-private-key*))
             (true (stringp pub1))
             (true (stringp priv1))
             (true (probe-file config:*vapid-keys-file*))
             ;; Clear in-memory keys and reload from disk
             (setf server:*vapid-public-key* nil
                   server:*vapid-private-key* nil)
             (server:load-or-create-vapid-keys)
             (is string= pub1 server:*vapid-public-key*)
             (is string= priv1 server:*vapid-private-key*)))
      (when (probe-file config:*vapid-keys-file*)
        (delete-file config:*vapid-keys-file*))
      (setf server:*vapid-public-key* saved-pub
            server:*vapid-private-key* saved-priv))))

(define-test push-worker-pool-lifecycle-and-bounded-queue
  :parent push-notification-tests
  (let ((delivered nil)
        (delivered-lock (bt:make-lock "test-delivered-lock"))
        (orig-deliver (symbol-function 'lisp-chat/server::deliver-push-job)))
    (unwind-protect
         (progn
           (server:stop-push-workers)
           ;; Mock delivery to capture delivered jobs without network calls
           (setf (symbol-function 'lisp-chat/server::deliver-push-job)
                 (lambda (job)
                   (bt:with-lock-held (delivered-lock)
                     (push (lisp-chat/server::push-job-session-id job) delivered))))
           ;; Start 2 workers (idempotent)
           (is = 2 (server:start-push-workers 2))
           (is = 2 (server:start-push-workers 2))
           ;; Enqueue 5 jobs and wait for worker pool to drain them
           (dotimes (i 5)
             (true (lisp-chat/server::enqueue-push-job
                    (lisp-chat/server::make-push-job
                     :session-id (format nil "sid-~a" i)
                     :sub-json "{}"
                     :payload "{}"
                     :vapid-public-key "pub"
                     :vapid-private-key "priv"
                     :vapid-subject "mailto:test@example.com"))))
           (loop repeat 40
                 while (< (bt:with-lock-held (delivered-lock) (length delivered)) 5)
                 do (sleep 0.05))
           (is = 5 (bt:with-lock-held (delivered-lock) (length delivered)))
           ;; Stop workers cleanly
           (server:stop-push-workers)
           (is eq nil lisp-chat/server::*push-workers*)
           ;; Verify bounded queue drops when full (with workers paused)
           (let ((server:*push-queue-max-size* 2))
             (setf lisp-chat/server::*push-workers-running* t)
             (true (lisp-chat/server::enqueue-push-job
                    (lisp-chat/server::make-push-job :session-id "q1")))
             (true (lisp-chat/server::enqueue-push-job
                    (lisp-chat/server::make-push-job :session-id "q2")))
             (false (lisp-chat/server::enqueue-push-job
                     (lisp-chat/server::make-push-job :session-id "q3")))
             ;; Drain queued items
             (is string= "q1" (lisp-chat/server::push-job-session-id (lisp-chat/server::dequeue-push-job)))
             (is string= "q2" (lisp-chat/server::push-job-session-id (lisp-chat/server::dequeue-push-job)))
             (is eq nil (lisp-chat/server::dequeue-push-job))
             (setf lisp-chat/server::*push-workers-running* nil)))
      (setf (symbol-function 'lisp-chat/server::deliver-push-job) orig-deliver)
      (server:stop-push-workers))))

(define-test atomic-sexp-persistence-leaves-no-tmp
  :parent push-notification-tests
  (let ((path "atomic-test.sexp")
        (tmp-path "atomic-test.sexp.tmp"))
    (unwind-protect
         (progn
           (lisp-chat/server::write-sexp-atomically path '(("alice" . ("sid-1"))))
           (true (probe-file path))
           (false (probe-file tmp-path))
           (lisp-chat/server::write-sexp-atomically path '(("alice" . ("sid-1" "sid-2"))))
           (false (probe-file tmp-path))
           (with-open-file (in path :direction :input)
             (is equal '(("alice" . ("sid-1" "sid-2"))) (read in))))
      (when (probe-file path) (delete-file path))
      (when (probe-file tmp-path) (delete-file tmp-path)))))
