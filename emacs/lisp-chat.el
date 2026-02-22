;;; lisp-chat.el --- Emacs client for Lisp Chat -*- lexical-binding: t; -*-

;; Author: Manoel V. Machado
;; Version: 0.7.0
;; Package-Requires: ((emacs "26.1") (websocket "1.12"))
;; Keywords: comm, chat, lisp

;;; Code:

(require 'websocket)
(require 'cl-lib)
(require 'browse-url)

(defgroup lisp-chat nil
  "Lisp Chat client settings."
  :group 'comm)

(defcustom lisp-chat-default-host "localhost"
  "Default host for Lisp Chat server."
  :type 'string
  :group 'lisp-chat)

(defcustom lisp-chat-default-port 8000
  "Default port for Lisp Chat server."
  :type 'integer
  :group 'lisp-chat)

(defcustom lisp-chat-default-url "wss://chat.manoel.dev/ws"
  "Default WebSocket URL for Lisp Chat server."
  :type 'string
  :group 'lisp-chat)

(defcustom lisp-chat-default-username nil
  "Default username for Lisp Chat. If nil, the user will be prompted."
  :type '(choice (const :tag "Prompt" nil)
                 (string :tag "Username"))
  :group 'lisp-chat)

(defvar lisp-chat-colors
  '("#ff7675" "#fab1a0" "#fdcb6e" "#e17055" "#d63031"
    "#00b894" "#00cec9" "#0984e3" "#6c5ce7" "#e84393"
    "#ffeaa7" "#55efc4" "#81ecec" "#74b9ff" "#a29bfe")
  "Colors used for usernames and mentions.")

(defvar lisp-chat-url-regexp
  "\\bhttps?://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"
  "Regular expression for matching URLs.")

(defvar lisp-chat-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'lisp-chat-browse-at-mouse)
    (define-key map [mouse-1] #'lisp-chat-browse-at-mouse)
    (define-key map (kbd "RET") #'lisp-chat-browse-at-point)
    map)
  "Keymap for links in Lisp Chat.")

(defun lisp-chat-browse-at-mouse (event)
  "Browse URL at mouse EVENT."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (lisp-chat-browse-at-point)))

(defun lisp-chat-browse-at-point ()
  "Browse URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'lisp-chat-url)))
    (when url
      (browse-url url))))

(defvar-local lisp-chat-connection nil)
(defvar-local lisp-chat-connection-type nil)
(defvar-local lisp-chat-username nil)
(defvar-local lisp-chat-users nil)
(defvar-local lisp-chat-ping-timer nil)
(defvar-local lisp-chat-last-date nil)
(defvar-local lisp-chat--tcp-buffer "")
(defvar-local lisp-chat--pending-username nil)
(defvar-local lisp-chat-input-marker nil)
(defvar-local lisp-chat-current-address nil)
(defvar-local lisp-chat-current-port nil)

;;; Internal Functions

(defun lisp-chat--to-int32 (x)
  "Convert X to a 32-bit signed integer."
  (let ((val (logand x #xFFFFFFFF)))
    (if (/= 0 (logand #x80000000 val)) (- val #x100000000) val)))

(defun lisp-chat--get-user-color (username)
  "Generate a color for USERNAME based on its hash."
  (cond
   ((or (null username) (string= username "")) "#888888")
   ((string= username "@server") "#bb2222")
   (t
    (let ((hash 0)
          (name (if (string-prefix-p "@" username) (substring username 1) username)))
      (dolist (char (append (string-to-list name) nil))
        (let ((shifted (lisp-chat--to-int32 (ash (lisp-chat--to-int32 hash) 12))))
          (setq hash (+ char (- shifted hash)))))
      (let* ((index (% (abs hash) (length lisp-chat-colors))))
        (nth index lisp-chat-colors))))))

(defun lisp-chat--format-message (text)
  "Format TEXT with colors for mentions and usernames, and clickable links."
  (let ((pos 0))
    ;; Links
    (while (string-match lisp-chat-url-regexp text pos)
      (let ((url (match-string 0 text))
            (start (match-beginning 0))
            (end (match-end 0)))
        (add-text-properties start end
                             `(face link
                               mouse-face highlight
                               help-echo ,(concat "mouse-1: browse " url)
                               keymap ,lisp-chat-link-keymap
                               lisp-chat-url ,url)
                             text)
        (setq pos end)))
    ;; Mentions
    (setq pos 0)
    (while (string-match "@\\([A-zÀ-ú0-9_.-]+\\)" text pos)
      (let* ((username (match-string 1 text))
             (color (lisp-chat--get-user-color (if (string= username "server") "@server" username))))
        (add-face-text-property (match-beginning 0) (match-end 0)
                                `(:foreground ,color :weight bold)
                                t text)
        (setq pos (match-end 0))))
    text))

(defun lisp-chat--insert-text (text &optional face)
  "Insert TEXT into the chat log area before the input marker with optional FACE."
  (with-current-buffer (get-buffer-create "*lisp-chat*")
    (let* ((inhibit-read-only t)
           (p-marker (marker-position lisp-chat-input-marker))
           (moving (and p-marker (>= (point) p-marker))))
      (save-excursion
        (when p-marker (goto-char p-marker))
        (let ((start (point)))
          (if face
              (insert (propertize text 'face face))
            (insert (lisp-chat--format-message text)))
          (unless (string-suffix-p "\n" text)
            (insert "\n"))
          (add-text-properties start (point) '(read-only t front-sticky t rear-nonsticky t))
          (set-marker lisp-chat-input-marker (point))))
      (when moving
        (goto-char (point-max))
        (walk-windows (lambda (window)
                        (when (eq (window-buffer window) (current-buffer))
                          (set-window-point window (point-max))))
                      nil t)))))

(defun lisp-chat--update-header-line ()
  "Update the header line with the list of online users."
  (let* ((sorted-users (sort (copy-sequence lisp-chat-users) #'string<))
         (user-strings (mapcar (lambda (u)
                                 (propertize u 'face `(:foreground ,(lisp-chat--get-user-color u) :weight bold)))
                               sorted-users))
         (header (concat " Online: " (mapconcat #'identity user-strings ", "))))
    (setq header-line-format header)
    (force-mode-line-update)))

(defun lisp-chat--update-prompt ()
  "Update the input prompt with the current username."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position lisp-chat-input-marker))
      (let ((p-end (field-end (point))))
        (when (> p-end (point))
          (delete-region (point) p-end)))
      (let* ((user (or lisp-chat-username lisp-chat-default-username "anonymous"))
             (color (lisp-chat--get-user-color user))
             (prompt-text (format "[%s]> " user))
             (prompt (propertize prompt-text
                                 'face `(:foreground ,color :weight bold)
                                 'read-only t
                                 'field t
                                 'inhibit-line-move-field-capture t
                                 'front-sticky t
                                 'rear-nonsticky t)))
        (insert prompt)))))

(defun lisp-chat--handle-server-message (message)
  "Process a MESSAGE from the server."
  (when (and message (not (string-empty-p message)))
    (with-current-buffer (get-buffer-create "*lisp-chat*")
      (dolist (line (split-string message "\r?\n" t))
        (cond
         ((string-match "^|\\(?:\\([0-9-]+\\) \\)?\\([0-9:]+\\)| \\[\\(.*?\\)\\]: \\(.*\\)$" line)
          (let* ((date (match-string 1 line))
                 (time (match-string 2 line))
                 (user (match-string 3 line))
                 (content (match-string 4 line))
                 (effective-date (or date (format-time-string "%Y-%m-%d")))
                 (user-color (lisp-chat--get-user-color user)))

            (when (and effective-date (not (equal effective-date lisp-chat-last-date)))
              (setq lisp-chat-last-date effective-date)
              (lisp-chat--insert-text (format "\n--- %s ---\n" effective-date) 'shadow))

            (when (string= user "@server")
              (cond
               ((string-match "The user @\\(.*?\\) joined to the party!" content)
                (let ((joined-user (match-string 1 content)))
                  (cl-pushnew joined-user lisp-chat-users :test #'string=)
                  (lisp-chat--update-header-line)
                  (when (and (null lisp-chat-username)
                             lisp-chat--pending-username
                             (string= joined-user lisp-chat--pending-username))
                    (setq lisp-chat-username joined-user)
                    (lisp-chat--update-prompt)
                    (lisp-chat-send "/users")
                    (lisp-chat-send "/log 100"))))
               ((string-match "The user @\\(.*?\\) exited from the party" content)
                (let ((exited-user (match-string 1 content)))
                  (setq lisp-chat-users (delete exited-user lisp-chat-users))
                  (lisp-chat--update-header-line)))
               ((string-prefix-p "users: " content)
                (setq lisp-chat-users (split-string (substring content 7) ", " t))
                (lisp-chat--update-header-line))
               ((string-match "Your new nick is: @\\(.*\\)" content)
                (let ((new-nick (match-string 1 content)))
                  (setq lisp-chat-users (delete lisp-chat-username lisp-chat-users))
                  (setq lisp-chat-username new-nick)
                  (cl-pushnew new-nick lisp-chat-users :test #'string=)
                  (lisp-chat--update-header-line)
                  (lisp-chat--update-prompt)))))

            (unless (and (string= user "@server") (string-match "pong (system)" content))
              (let ((timestamp (propertize (concat "[" time "] ") 'face 'shadow))
                    (username (propertize (concat "[" user "]: ") 'face `(:foreground ,user-color :weight bold)))
                    (body (lisp-chat--format-message content)))
                (lisp-chat--insert-text (concat timestamp username body))))))

         ((string-match "> Type your username: " line)
          (if lisp-chat-default-username
              (progn
                (lisp-chat--insert-text (format "Logging in as %s..." lisp-chat-default-username) 'italic)
                (setq lisp-chat--pending-username lisp-chat-default-username)
                (lisp-chat-send lisp-chat-default-username))
            (lisp-chat--insert-text "Connected! Please type your username below and press Enter." 'italic)))

         ((string-match "> Name cannot be empty. Try again: " line)
          (lisp-chat--insert-text "Name cannot be empty. Try again below." 'warning))

         ((not (string-match-p "pong" line))
          (lisp-chat--insert-text (lisp-chat--format-message line))))))))

(defun lisp-chat--tcp-filter (proc string)
  "Filter for TCP process PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (setq lisp-chat--tcp-buffer (concat lisp-chat--tcp-buffer string))
      (while (string-match "\n" lisp-chat--tcp-buffer)
        (let ((line (substring lisp-chat--tcp-buffer 0 (match-beginning 0))))
          (setq lisp-chat--tcp-buffer (substring lisp-chat--tcp-buffer (match-end 0)))
          (lisp-chat--handle-server-message line))))))

;;; Public Commands

(defun lisp-chat-clear ()
  "Clear the chat area."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (marker-position lisp-chat-input-marker))
    (setq lisp-chat-last-date nil)))

(defun lisp-chat-reconnect (&optional attempt)
  "Reconnect to the server, trying up to 10 times."
  (interactive)
  (let ((address lisp-chat-current-address)
        (port lisp-chat-current-port)
        (username (or lisp-chat-username lisp-chat-default-username))
        (curr-attempt (or attempt 1)))
    (if (> curr-attempt 10)
        (lisp-chat--insert-text "Reconnection failed after 10 attempts." 'error)
      (when address
        (when lisp-chat-ping-timer
          (cancel-timer lisp-chat-ping-timer)
          (setq lisp-chat-ping-timer nil))
        (when lisp-chat-connection
          (ignore-errors
            (if (eq lisp-chat-connection-type 'websocket)
                (websocket-close lisp-chat-connection)
              (delete-process lisp-chat-connection)))
          (setq lisp-chat-connection nil))
        
        (lisp-chat-clear)
        (setq lisp-chat-username nil
              lisp-chat-users nil)
        (lisp-chat--update-header-line)
        (lisp-chat--update-prompt)
        (lisp-chat--insert-text (format "Reconnecting... (attempt %d/10)" curr-attempt) 'warning)
        
        (condition-case err
            (progn
              (if (eq lisp-chat-connection-type 'websocket)
                  (setq lisp-chat-connection
                        (websocket-open
                         address
                         :on-message (lambda (_ws frame) (lisp-chat--handle-server-message (websocket-frame-text frame)))
                         :on-close (lambda (_ws) (message "Lisp Chat: Connection closed."))
                         :on-error (lambda (_ws _type err) (message "Lisp Chat: WebSocket error: %S" err))))
                (setq lisp-chat-connection
                      (make-network-process
                       :name "lisp-chat-tcp" :buffer (current-buffer) :host address :service port
                       :filter #'lisp-chat--tcp-filter
                       :sentinel (lambda (proc event) (when (string-match-p "finished" event) (message "Lisp Chat: TCP Connection closed."))))))
              (lisp-chat--start-ping-timer)
              (when username
                (setq lisp-chat-default-username username)
                (setq lisp-chat--pending-username username)))
          (error
           (run-at-time 3 nil (lambda (buf next-attempt)
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (lisp-chat-reconnect next-attempt))))
                        (current-buffer) (1+ curr-attempt))))))))

(defun lisp-chat-send (message)
  "Send MESSAGE to the server."
  (when (and lisp-chat-connection message (not (string-empty-p message)))
    (condition-case err
        (if (eq lisp-chat-connection-type 'websocket)
            (websocket-send-text lisp-chat-connection message)
          (process-send-string lisp-chat-connection (concat message "\n")))
      (error
       (message "Lisp Chat: Send error: %S" err)
       (lisp-chat-reconnect)))))

(defun lisp-chat-ret ()
  "Handle pressing Enter in the chat buffer."
  (interactive)
  (let ((p-end (field-end (marker-position lisp-chat-input-marker))))
    (if (< (point) p-end)
        (goto-char (point-max))
      (let* ((message (buffer-substring p-end (point-max))))
        (unless (string-empty-p (string-trim message))
          (let ((inhibit-read-only t))
            (delete-region p-end (point-max)))
          (let ((trimmed (string-trim message)))
            (cond
             ((string= trimmed "/quit")
              (lisp-chat-quit))
             ((string= trimmed "/clear")
              (lisp-chat-clear))
             (t
              (if (null lisp-chat-username)
                  (progn
                    (setq lisp-chat--pending-username trimmed)
                    (lisp-chat-send lisp-chat--pending-username))
                (lisp-chat-send trimmed))))))))))

(defun lisp-chat-quit ()
  "Disconnect from the server and close the buffer."
  (interactive)
  (when lisp-chat-ping-timer (cancel-timer lisp-chat-ping-timer))
  (when lisp-chat-connection
    (if (eq lisp-chat-connection-type 'websocket) (websocket-close lisp-chat-connection)
      (delete-process lisp-chat-connection)))
  (kill-buffer (current-buffer)))

(defun lisp-chat--ensure-point ()
  "Ensure the cursor stays after the prompt when typing, but allow navigation."
  (let ((p-marker (marker-position lisp-chat-input-marker)))
    (when (and p-marker
               (not (memq this-command '(previous-line next-line backward-char forward-char
                                         scroll-up-command scroll-down-command
                                         mwheel-scroll beginning-of-buffer end-of-buffer
                                         move-beginning-of-line move-end-of-line
                                         mouse-set-point mouse-drag-region
                                         isearch-forward isearch-backward)))
               (< (point) p-marker))
      (goto-char (point-max)))))

;;; Mode Definition

(defvar lisp-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lisp-chat-ret)
    (define-key map (kbd "C-m") 'lisp-chat-ret)
    (define-key map (kbd "C-c C-q") 'lisp-chat-quit)
    map))

(define-derived-mode lisp-chat-mode fundamental-mode "Lisp-Chat"
  "Major mode for Lisp Chat client."
  (setq-local lisp-chat-users nil
              lisp-chat-username nil
              lisp-chat-last-date nil
              lisp-chat--tcp-buffer ""
              lisp-chat--pending-username nil)
  (setq-local scroll-up-aggressively 0.1
              scroll-conservatively 101)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq lisp-chat-input-marker (make-marker))
    (set-marker lisp-chat-input-marker (point-max))
    (lisp-chat--update-prompt)
    (goto-char (point-max)))
  (lisp-chat--update-header-line)
  (add-hook 'post-command-hook #'lisp-chat--ensure-point nil t))

;;; Connection Entry Points

(defun lisp-chat--start-ping-timer ()
  "Start a timer to send pings every 30 seconds."
  (setq lisp-chat-ping-timer
        (run-at-time 30 30
                     (lambda (buf)
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (when (and lisp-chat-connection lisp-chat-username)
                             (lisp-chat-send "/ping system")))))
                     (current-buffer))))

(defun lisp-chat-connect-websocket (url)
  "Connect to Lisp Chat server via WebSocket URL."
  (let ((buf (get-buffer-create "*lisp-chat*")))
    (with-current-buffer buf
      (lisp-chat-mode)
      (setq lisp-chat-current-address url)
      (setq lisp-chat-connection-type 'websocket
            lisp-chat-connection
            (websocket-open
             url
             :on-message (lambda (_ws frame) (lisp-chat--handle-server-message (websocket-frame-text frame)))
             :on-close (lambda (_ws) (message "Lisp Chat: Connection closed."))
             :on-error (lambda (_ws _type err) (message "Lisp Chat: WebSocket error: %S" err))))
      (lisp-chat--start-ping-timer))
    (switch-to-buffer buf)))

(defun lisp-chat-connect-tcp (host port)
  "Connect to Lisp Chat server via TCP."
  (let ((buf (get-buffer-create "*lisp-chat*")))
    (with-current-buffer buf
      (lisp-chat-mode)
      (setq lisp-chat-current-address host
            lisp-chat-current-port port)
      (setq lisp-chat-connection-type 'tcp
            lisp-chat-connection
            (make-network-process
             :name "lisp-chat-tcp" :buffer buf :host host :service port
             :filter #'lisp-chat--tcp-filter
             :sentinel (lambda (proc event) (when (string-match-p "finished" event) (message "Lisp Chat: TCP Connection closed.")))))
      (lisp-chat--start-ping-timer))
    (switch-to-buffer buf)))

;;;###autoload
(defun lisp-chat (address &optional port)
  "Connect to Lisp Chat at ADDRESS.
If ADDRESS starts with ws:// or wss://, use WebSockets.
Otherwise, use TCP on PORT (default 8000)."
  (interactive (list (read-string "Address: " lisp-chat-default-url)))
  (cond
   ((string-match-p "^ws\\(s\\)?://" address) (lisp-chat-connect-websocket address))
   (t (lisp-chat-connect-tcp address (or port (read-number "Port: " lisp-chat-default-port))))))

(provide 'lisp-chat)
