
(defun grovel-file (path)
  (with-open-file (stream path :element-type 'character)
    (
(defun grovel (path)
  (labels ((dir (d)
             (dolist (f d)
               (unless (find (namestring f) '("." ".."))
                 (if (pathname-name f)
                     (file f)
                     (dir f)))))
           (file (f)
             (unless (not (string-equal "lisp" (pathname-type f)))
               (grovel-file f))))
    (dir path)))
