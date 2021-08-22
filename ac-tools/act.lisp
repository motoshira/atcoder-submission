(defparameter *template-dir* "~/ghq/github.com/motoshira/atcoder-submission/template/main.lisp")

#+swank
(defun get-clipboard ()
  (with-output-to-string (out)
    (sb-ext:run-program "/usr/bin/pbpaste" '() :output out)))

#+swank
(defun run ()
  (with-input-from-string (*standard-input* (get-clipboard))
    (main)))

#+swank
(defun copy-flie (from to)
  (with-open-file (out to
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-open-file (in from
                        :direction :input)
      (loop for line = (read-line in nil :eof)
            until (eq line :eof)
            do (princ line out)
               (terpri out)))))

(defun mkdir (folder-name)
  (ensure-directories-exist (merge-pathnames folder-name (truename "."))))

#+swank
(defun make-project (project-name &optional (child-filenames '("a" "b" "c" "d" "e" "f" "g" "h")))
  (flet ((println (obj)
           (princ obj)
           (terpri)))
    ;; make parent-dir
    (mkdir (concatenate 'string project-name "/"))
    (format t "Creating ~a..." (truename (concatenate 'string project-name "/")))
    (println "Done.")
    (dolist (c child-filenames)
      (let ((child-dir (mkdir (concatenate 'string project-name "/" c "/"))))
        (format t "Copying ~a..." (merge-pathnames "main.lisp" child-dir))
        (copy-flie *template-dir*
                   (merge-pathnames "main.lisp" child-dir))
        (println "Done.")))))
