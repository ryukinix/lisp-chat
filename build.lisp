(pushnew (truename (sb-unix:posix-getcwd/)) ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload '(:lisp-chat :cffi))

(defun main()
  (let ((libpath (sb-posix:getcwd)))
    (format t "=> New LD_LIBRARY_PATH: ~a~%" libpath)
    (pushnew libpath
             cffi:*foreign-library-directories*
             :test #'equal)
    (cffi:define-foreign-library libreadline
      (t "libreadline.so"))
    (cffi:use-foreign-library libreadline))
  (lisp-chat-client:main))

;; close currently foreign libraries loaded
(loop for library in (cffi:list-foreign-libraries :loaded-only t)
      do (cffi:close-foreign-library library))

(sb-ext:save-lisp-and-die "lisp-chat"
                          :toplevel #'main
                          :executable t
                          :compression 1)
