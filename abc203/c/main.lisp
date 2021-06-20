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

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(defparameter *max* (expt 10 100))

(defun main ()
  (let* ((n (read))
         (k (read))
         (ls (loop repeat n collect (list (read) (read)))))
    (setf ls (sort ls #'< :key #'first))
    (println (nlet rec ((res 0)
                        (rest k)
                        (ls ls))
                   (if (null ls)
                       (min *max*
                            (+ res rest))
                       (destructuring-bind (point money)
                           (first ls)
                         (let ((need (- point res)))
                           (if (>= rest need)
                               (rec point
                                    (+ money (- rest need))
                                    (rest ls))
                               (+ res rest)))))))))

#-swank (main)
