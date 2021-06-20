(in-package :cl-user)

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

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defun main ()
  (let* ((n (read))
         (as (loop repeat n collect (read)))
         (counter (make-hash-table)))
    (dolist (a as)
      (incf (gethash a counter 0)))
    (let ((res (ash (* n (1- n)) -1)))
      (maphash (lambda (val cnt)
                 (declare (ignorable val))
                 (decf res (ash (* cnt (1- cnt))
                                -1)))
               counter)
      (println res))))

#-swank (main)
