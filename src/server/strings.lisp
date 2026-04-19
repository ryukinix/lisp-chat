(in-package :lisp-chat/server)

(defun separator-p (c)
  (eql c #\Space))

(defun split-quotation-aware (string delimiterp)
  "Split a string preserving quotation as single tokens"
  (let ((tokens nil)
        (token (make-array 10 :element-type 'character :adjustable t :fill-pointer 0))
        (quote-char nil))
    (loop for char across string
          do (cond
               ((and quote-char (char= char quote-char))
                (setf quote-char nil))
               ((and (not quote-char) (or (char= char #\") (char= char #\')))
                (setf quote-char char))
               ((and (not quote-char) (funcall delimiterp char))
                (when (plusp (length token))
                  (push (copy-seq token) tokens)
                  (setf (fill-pointer token) 0)))
               (t (vector-push-extend char token))))
    (when (plusp (length token))
      (push (copy-seq token) tokens))
    (nreverse tokens)))

(defun split-with-empty-seqs (string delimiterp)
  "Slit a string maintaing empty strings when there is multiple consecutive delimiters"
  (loop for start = 0 then (1+ pos)
        for pos = (position-if delimiterp string :start start)
        collect (subseq string start pos)
        while pos))

(defun split-trivial (string delimiterp)
  (loop for beg = (position-if-not delimiterp string)
          then (position-if-not delimiterp string :start (1+ end))
        for end = (and beg (position-if delimiterp string :start beg))
        when beg
          collect (subseq string beg end)
        while end))

(defun split (string &key (delimiterp #'separator-p) quotation-aware empty-seqs)
  "Split a string by a delimiterp function character checking"
  (cond
    ((and quotation-aware empty-seqs)
     (error "QUOTATION-AWARE and WITH-EMTPY-SEQS cannot be used together"))
    (quotation-aware (split-quotation-aware string delimiterp))
    (empty-seqs (split-with-empty-seqs string delimiterp))
    (t (split-trivial string delimiterp))))

(defun startswith (string substring)
  "Check if STRING starts with SUBSTRING."
  (let ((l1 (length string))
        (l2 (length substring)))
    (when (and (> l2 0)
               (>= l1 l2))
      (loop for c1 across string
            for c2 across substring
            always (equal c1 c2)))))
