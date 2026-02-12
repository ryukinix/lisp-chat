(in-package :lisp-chat/tests)

(define-test unit-tests
  :parent lisp-chat-tests)

(define-test time-formatting
  :parent unit-tests
  (let ((time '(0 30 12 11 2 2026 2 nil 0))) ;; 2026-02-11 12:30:00 Wednesday GMT+0
    (is string= "12:30:00 of Wednesday, 2026-02-11 (GMT+0)"
        (lisp-chat/server:format-time time))
    (let ((msg (lisp-chat/server:make-message :from "test" :content "hi" :time time)))
      (is string= "12:30:00" (lisp-chat/server:message-time-hour-format msg))
      (is string= "2026-02-11 12:30:00" (lisp-chat/server:message-time-date-format msg)))))

(define-test system-version-parsing
  :parent unit-tests
  (let ((version-simple "0.1.0")
        (version-rc "0.1.0-rc1")
        (version-simple-with-build "0.1.0+20260101"))
    (is string= "0.1.0" (lisp-chat/system::lisp-chat-parse-version version-simple))
    (is string= "-rc1" (lisp-chat/system::lisp-chat-parse-build-metadata version-rc))
    (is string= "+20260101" (lisp-chat/system::lisp-chat-parse-build-metadata version-simple-with-build))))
