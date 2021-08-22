#-swank
(unless (member :child-sbcl *features*)
  (quit
   :recklessly-p t
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "256MB"
                                          "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                                          "--eval" "(push :child-sbcl *features*)"
                                          "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))

(in-package #:cl-user)

;;------------------------------Preferences------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+swank (declaim (optimize (speed 3) (safety 2)))
  #-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/ghq/github.com/motoshira/atcoder-submission/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

;;---------------------------------Body---------------------------------

(in-package #:cl-user)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defun main ()
  (let* ((tmp (read-line))
         (pos (position #\Space tmp))
         (s (subseq tmp 0 pos))
         (k (parse-integer (subseq tmp (1+ pos))))
         (cand nil))
    (sb-int:named-let rec ((xs (concatenate 'list s))
                           (acc nil))
      (if (null xs)
          (push (concatenate 'string (reverse acc))
                cand)
          (dolist (x xs)
            (rec (remove x xs :count 1)
                 (cons x acc)))))
    (println (nth (1- k)
                  (sort (remove-duplicates cand :test #'equal)
                        #'string<)))))

#-swank (main)
