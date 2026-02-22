(in-package :lisp-chat/tests)

(define-test integration-tests
  :parent lisp-chat-tests)

(define-test (integration-tests :before)
    (lisp-chat/server:reset-server))

(defun get-current-date ()
    (multiple-value-bind
          (second minute hour date month year)
        (get-decoded-time)
      (declare (ignore second minute hour))
      (format nil "~4d-~2,'0d-~2,'0d"
              year month date)))

;;; Declarative Testing Helpers

(defmacro with-tcp-client ((stream) &body body)
  (let ((socket (gensym)))
    `(let* ((,socket (usocket:socket-connect "127.0.0.1" *port*))
            (,stream (usocket:socket-stream ,socket)))
       (unwind-protect
            (progn ,@body)
         (usocket:socket-close ,socket)))))

(defmacro with-websocket-client ((client messages-var) &body body)
  (let ((url (gensym))
        (connected (gensym)))
    `(let* ((,url (format nil "ws://127.0.0.1:~a/ws" *websocket-port*))
            (,client (make-client ,url))
            (,connected nil)
            (,messages-var '()))
       (on :open ,client (lambda () (setf ,connected t)))
       (on :message ,client (lambda (msg) (push msg ,messages-var)))
       (start-connection ,client)
       (loop repeat 20 until ,connected do (sleep 0.1))
       (unwind-protect
            (progn ,@body)
         (close-connection ,client)))))

(defun tcp-interaction (stream &rest steps)
  "Execute a sequence of steps:
   - string: send line to server
   - (:expect pattern): wait and check if line contains pattern
   - (:ignore [n]): read and discard n lines (default 1)
   - (:sleep n): sleep for n seconds"
  (dolist (step steps)
    (cond
      ((stringp step)
       (write-line step stream)
       (finish-output stream))
      ((and (listp step) (eq (car step) :expect))
       (let ((line (read-line stream)))
         (when *debug* (format t "RECV: ~A~%" line))
         (true (search (second step) line))))
      ((and (listp step) (eq (car step) :ignore))
       (loop repeat (or (second step) 1) do (read-line stream)))
      ((and (listp step) (eq (car step) :wait-for))
       (let ((pattern (second step))
             (found nil))
         (loop repeat 10 do
           (let ((line (read-line stream)))
             (when (search pattern line)
               (setf found t)
               (return))))
         (true found "Pattern '~A' not found within limit." pattern)))
      ((and (listp step) (eq (car step) :sleep))
       (sleep (second step))))))

(defun ws-interaction (client messages-fn &rest steps)
  (dolist (step steps)
    (cond
      ((stringp step)
       (send client step))
      ((and (listp step) (eq (car step) :expect))
       (let ((pattern (second step))
             (found nil))
         ;; Poll for message
         (loop repeat 30 do
           (when (some (lambda (m) (search pattern m)) (funcall messages-fn))
             (setf found t)
             (return))
           (sleep 0.1))
         (true found "Pattern '~A' not found. Messages received: ~S" pattern (funcall messages-fn))))
      ((and (listp step) (eq (car step) :sleep))
       (sleep (second step))))))

;;; Tests

(define-test tcp-client-connection
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-tcp"
      '(:expect "The user @tester-tcp joined to the party!"))))

(define-test websocket-client-connection
  :parent integration-tests
  (with-websocket-client (client messages)
    (ws-interaction client (lambda () messages)
      '(:expect "Type your username")
      "tester-ws"
      '(:expect "The user @tester-ws joined to the party!"))))

(define-test websocket-ping-command
  :parent integration-tests
  (with-websocket-client (client messages)
    (ws-interaction client (lambda () messages)
      '(:expect "Type your username")
      "tester-ping"
      '(:expect "The user @tester-ping joined to the party!")
      '(:sleep 0.2)
      "/ping"
      '(:expect "latency:"))))

(define-test log-commands-with-date-format
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-log"
      '(:wait-for "tester-log joined") ;; wait for join message
      "hello log"
      '(:wait-for "hello log") ;; wait for broadcast of our message
      "/log :date-format date"
      `(:expect ,(get-current-date)))))

(define-test lisp-command-with-timeout
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-lisp"
      '(:ignore 1) ;; welcome message
      "/lisp (dotimes (x 1000) (when (eq x 2) (setq x 1)))"
      '(:sleep 1)
      '(:expect "TIMEOUT: Timeout occurred after 0.5 seconds"))))

(define-test lisp-command-with-loop
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-lisp"
      '(:ignore 1) ;; welcome message
      "/lisp (loop for x from 1 to 5 collect x)"
      '(:sleep 0.1)
      '(:expect "(1 2 3 4 5)"))))

(define-test search-command-basic
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-search"
      '(:wait-for "tester-search joined")
      "needle in a haystack"
      '(:expect "]: needle in a haystack") ;; Wait for broadcast
      "/search needle"
      '(:expect "needle in a haystack"))))

(define-test search-command-with-user
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-user"
      '(:wait-for "tester-user joined")
      "message from user"
      '(:expect "]: message from user") ;; Wait for broadcast
      "/search message :user tester-user"
      '(:expect "[search:tester-user]: message from user")
      "/search message :user other-user"
      '(:expect "")))) ;; Should not find anything

(define-test search-command-with-limit
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-limit"
      '(:wait-for "tester-limit joined")
      "msg1" '(:expect "]: msg1")
      "msg2" '(:expect "]: msg2")
      "msg3" '(:expect "]: msg3")
      "/search msg :limit 2"
      '(:expect "msg2")
      '(:expect "msg3"))))



(define-test search-command-with-date-filters
  :parent integration-tests
  (let ((today (get-current-date)))
    (with-tcp-client (stream)
      (tcp-interaction stream
        '(:expect "> Type your username: ")
        "tester-date"
        '(:wait-for "tester-date joined")
        "dated message"
        '(:wait-for "dated message")
        (format nil "/search dated :after ~a" today)
        '(:expect "[search:tester-date]: dated message")
        "/search dated :before 2000-01-01"
        '(:expect ""))))) ;; Should not find anything

(define-test lisp-command-with-keywords
  :parent integration-tests
  (with-tcp-client (stream)
    (tcp-interaction stream
      '(:expect "> Type your username: ")
      "tester-lisp"
      '(:ignore 1) ;; welcome message
      "/lisp (print :KEYWORD)"
      '(:sleep 0.1)
      '(:expect ":KEYWORD"))))
