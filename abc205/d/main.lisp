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

(defun lower-bound (fn ok ng)
  (if (<= (abs (- ok ng)) 1)
      ok
      (let ((mid (ash (+ ok ng) -1)))
        (if (funcall fn mid)
            (lower-bound fn mid ng)
            (lower-bound fn ok mid)))))

(defun main ()
  (let* ((n (read))
         (q (read))
         (as (loop repeat n collect (read)))
         (bs (coerce (sort (copy-seq as) #'<) 'vector))
         (qs (loop repeat q collect (read))))
    (dolist (k qs)
      (println (lower-bound (lambda (x)
                              (let ((cnt (lower-bound (lambda (idx)
                                                        (<= (aref bs idx)
                                                            x))
                                                      0
                                                      (1+ n))))
                                (< cnt k)))
                            0
                            (expt 10 20))))))


#-swank (main)
