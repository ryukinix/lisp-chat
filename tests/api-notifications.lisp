(in-package :lisp-chat/tests)

(define-test notifications-api
  :parent api-tests
  (server:reset-server)
  (server:push-notification "user1" "sender" "hello @user1")
  (let ((url (format nil "http://~a:~a/api/notifications?user=user1" config:*host* config:*websocket-port*)))
    (multiple-value-bind (body status) (dex:get url)
      (is = 200 status)
      (let* ((data (yason:parse body))
             (notifications (gethash "notifications" data)))
        (is = 1 (length notifications))
        (let ((n (car notifications)))
          (is string= "sender" (gethash "from" n))
          (is string= "hello @user1" (gethash "content" n))))))

  ;; Test clearing
  (let ((url (format nil "http://~a:~a/api/notifications?user=user1&clear=true" config:*host* config:*websocket-port*)))
    (multiple-value-bind (body status) (dex:get url)
      (declare (ignore status))
      (let* ((data (yason:parse body))
             (notifications (gethash "notifications" data)))
        (is = 1 (length notifications)))))

  ;; Verify it's cleared
  (let ((url (format nil "http://~a:~a/api/notifications?user=user1" config:*host* config:*websocket-port*)))
    (multiple-value-bind (body status) (dex:get url)
      (declare (ignore status))
      (let* ((data (yason:parse body))
             (notifications (gethash "notifications" data)))
        (is = 0 (length notifications))))))
