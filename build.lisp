(pushnew (truename (sb-unix:posix-getcwd/)) ql:*local-project-directories* )
(ql:register-local-projects)
(ql:quickload :lisp-chat)
(sb-ext:save-lisp-and-die "lisp-chat"
                          :toplevel #'lisp-chat-client:main
                          :executable t
                          :compression 9)
