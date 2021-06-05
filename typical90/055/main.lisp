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
         (p (read))
         (q (read))
         (as (loop repeat n collect (read))))
    (let ((stack nil)
          (res 0))
      (push (list 1 0) stack)
      (dolist (a as)
        (let ((tmp nil))
          (loop for (val cnt) in stack
                do (let ((next (rem (* val a)
                                    p)))
                     (cond
                       ((< cnt 4)
                        (push (list next (1+ cnt))
                              tmp))
                       (:else
                        (when (= next q)
                          (incf res))))))
          (dolist (xs tmp)
            (push xs stack))))
      (println res))))

#-swank (main)
