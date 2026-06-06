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
