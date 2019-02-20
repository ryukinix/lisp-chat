(pushnew (truename (sb-unix:posix-getcwd/)) ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload '(:lisp-chat :cffi))

(eval-when (:compile-toplevel :execute)
    (defconstant +debug+ nil))

(defmacro debug-format (&rest body)
  (when +debug+
    `(format t ,@body)))

(defun import-foreign-libraries ()
  (let ((libpath (uiop/os:getcwd)))
    (debug-format "=> New LD_LIBRARY_PATH: ~a~%" libpath)
    (pushnew libpath
             cffi:*foreign-library-directories*
             :test #'equal)
    (cffi:define-foreign-library libreadline
      (t "libreadline.so"))
    (cffi:use-foreign-library libreadline))
  (debug-format "Loaded libraries: ~a~%" (cffi:list-foreign-libraries)))


(defun main()
  (import-foreign-libraries)
  (lisp-chat-client:main))

;; close currently foreign libraries loaded
(loop for library in (cffi:list-foreign-libraries :loaded-only t)
      do (cffi:close-foreign-library library))

(sb-ext:save-lisp-and-die "lisp-chat"
                          :toplevel #'main
                          :executable t
                          :compression 1)
