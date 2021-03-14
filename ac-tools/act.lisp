(defvar *template-dir* "~/Dropbox/template/main.lisp")

#+swank
(defun get-clipboard ()
  (with-output-to-string (out)
    (sb-ext:run-program "/usr/bin/xclip" '("-o") :output out)))

#+swank
(defun run ()
  (with-input-from-string (in (get-clipboard))
    (let ((dat (loop for line = (read-line in nil :eof)
                     for n = (if (eq line :eof) 0 (length line))
                     until (eq line :eof)
                     collect (format nil "~a~%" line)
                         into res
                     finally (return (apply #'concatenate 'string res)))))
      (with-input-from-string (*standard-input* dat)
        (main)))))

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
(defun make-project (project-name &optional (child-filenames '("a" "b" "c" "d" "e" "f")))
  (flet ((println (obj)
           (princ obj)
           (terpri)))
    ;; make parent-dir
    (mkdir (concatenate 'string project-name "/"))
    (format t "Creating ~a..." (truename (concatenate 'string project-name "/")))
    (println "Done.")
    (dolist (c child-filenames)
      a        (let ((child-dir (mkdir (concatenate 'string project-name "/" c "/"))))
                 (format t "Copying ~a..." (merge-pathnames "main.lisp" child-dir))
                 (copy-flie *template-dir*
                            (merge-pathnames "main.lisp" child-dir))
                 (println "Done.")))))
