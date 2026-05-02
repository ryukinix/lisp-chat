(in-package :lisp-chat/tests)

(define-test mention-extraction
  :parent unit-tests
  (is equal '("user1") (server:extract-mentions "hello @user1"))
  (is equal '("user1" "user2") (server:extract-mentions "hello @user1 and @user2"))
  (is equal '("user1") (server:extract-mentions "hello @user1!"))
  (is equal '("user1") (server:extract-mentions "@user1: how are you?"))
  (is equal '("user1") (server:extract-mentions "cc @user1,"))
  (is equal '() (server:extract-mentions "email at user@example.com"))
  (is equal '() (server:extract-mentions "just @ alone"))
  (is equal '("user1") (server:extract-mentions "@user1 @user1")))

(define-test notification-storage
  :parent unit-tests
  (server:reset-server)
  (server:push-notification "user1" "sender" "hello @user1")
  (bt:with-lock-held (server::*notifications-lock*)
    (let ((notifications (gethash "user1" server::*notifications*)))
      (is equal 1 (length notifications))
      (let ((n (car notifications)))
        (is string= "sender" (server:notification-from n))
        (is string= "hello @user1" (server:notification-content n))))))
