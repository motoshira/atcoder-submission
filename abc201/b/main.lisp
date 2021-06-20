(in-package :cl-user)

;;------------------------------Preferences------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+swank (declaim (optimize (speed 3) (safety 2)))
  #-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

;;---------------------------------Body---------------------------------

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defun main ()
  (let* ((n (read))
         (ls (loop repeat n
                   for tmp = (read-line)
                   for pos = (position #\Space tmp)
                   do (assert pos)
                   collect (list (subseq tmp 0 pos)
                                 (parse-integer (subseq tmp (1+ pos)))))))
    (println (first (second (sort ls #'> :key #'second))))))

#-swank (main)
