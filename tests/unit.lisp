(in-package :lisp-chat/tests)

(define-test unit-tests
  :parent lisp-chat-tests)

(define-test time-formatting
  :parent unit-tests
  (let ((time '(0 30 12 11 2 2026 2 nil 0))) ;; 2026-02-11 12:30:00 Wednesday GMT+0
    (is string= "12:30:00 of Wednesday, 2026-02-11 (GMT+0)"
        (server:format-time time))
    (let ((msg (server:make-message :from "test" :content "hi" :time time)))
      (is string= "12:30:00" (server:message-time-hour-format msg))
      (is string= "2026-02-11 12:30:00" (server:message-time-date-format msg)))))

(define-test system-version-parsing
  :parent unit-tests
  (let ((version-simple "0.1.0")
        (version-rc "0.1.0-rc1")
        (version-simple-with-build "0.1.0+20260101"))
    (is string= "0.1.0" (lisp-chat/system::lisp-chat-parse-version version-simple))
    (is string= "-rc1" (lisp-chat/system::lisp-chat-parse-build-metadata version-rc))
    (is string= "+20260101" (lisp-chat/system::lisp-chat-parse-build-metadata version-simple-with-build))))


(define-test commands-string-utils
  :parent unit-tests
  (is equal
      '("x" "y" "z")
      (server:split "x.y.z"
                    :delimiterp (lambda (c) (eql c #\.))))
  (is equal
      '("foo" "bar zoo")
      (server:split "foo 'bar zoo'" :quotation-aware t))
  (is equal
      '("foo" "" "bar")
      (server:split "foo  bar" :empty-seqs t))
  (true (server:startswith "/command" "/")))

(define-test message-formatting
  :parent unit-tests
  (let* ((message (server:make-message :from "@server"
                                       :content (format nil "line1~%~%xline2")
                                       :time (server:get-time)))
         (message-string (server:formatted-message message :client nil)))
   (is equal
       3
       (count #\@ message-string :test #'char-equal))))

(define-test channel-normalization
  :parent unit-tests
  (is string= "#general" (server:normalize-channel "general"))
  (is string= "#general" (server:normalize-channel "#general"))
  (is string= "#general" (server:normalize-channel "%23general"))
  (is string= "#general" (server:normalize-channel "  #general  "))
  (is string= "#general" (server:normalize-channel "%23#general"))
  (is string= "#foo-bar" (server:normalize-channel "foo bar"))
  (is string= "#foo-bar" (server:normalize-channel "foo_bar"))
  (is string= "#foo-bar" (server:normalize-channel "fóó bãr"))
  (is string= "#foobar" (server:normalize-channel "foo!@#bar"))
  (is string= "#general" (server:normalize-channel ""))
  (is string= "#general" (server:normalize-channel "  "))
  (is string= "#general" (server:normalize-channel "#"))
  (is string= "#general" (server:normalize-channel "%23"))
  (is eq nil (server:normalize-channel nil)))

(define-test message-references
  :parent unit-tests
  (server:reset-server)
  (let* ((time (server:get-time))
         (msg (server:make-message :from "user1" :content "hello" :channel "#general" :time time))
         (date-str (server:message-time-date-format msg))
         (ref-string (format nil "<#general: ~a [user1]>" date-str)))
    (push msg server:*messages-log*)
    (let ((found (server:get-message-by-reference-string ref-string)))
      (true found)
      (is string= "user1" (server:message-from found))
      (is string= "hello" (server:message-content found)))))

(define-test log-command-with-reference
  :parent unit-tests
  (server:reset-server)
  (let ((client (server::make-client :active-channel "#general")))
    (dotimes (i 50)
      (push (server:make-message :from (format nil "user~a" i)
                                 :content (format nil "msg ~a" i)
                                 :channel "#general"
                                 :time (server:get-time))
            server:*messages-log*))
    (let* ((mid-msg (nth 25 (reverse server:*messages-log*)))
           (date-str (server:message-time-date-format mid-msg))
           (ref-string (format nil "<#general: ~a [~a]>" date-str (server:message-from mid-msg)))
           (result (lisp-chat/commands:call-command client (format nil "/log :reference \"~a\" :depth 10" ref-string))))
      (true result)
      ;; Result should contains about 10 messages around msg 25
      (is equal 10 (length (server:split result :delimiterp (lambda (c) (char= c #\Newline)) :empty-seqs t))))))


