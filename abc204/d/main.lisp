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

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defparameter *max* (expt 10 6))
(defparameter *inf* (expt 10 12))

(define-modify-macro minf (var) min)

(defun main ()
  (declare (inline sort))
  (let* ((n (read))
         (ts (loop repeat n collect (read)))
         (m (reduce #'+ ts))
         (dp (make-array (list (1+ n) (1+ m))
                         :initial-element nil)))
    (setf (aref dp 0 0) t)
    (dotimes (i n)
      (let ((time (pop ts)))
        (dotimes (x (1+ m))
          (when (aref dp i x)
            (setf (aref dp (1+ i) x)
                  t)
            (let ((nt (+ x time)))
              (when (<= nt m)
                (setf (aref dp (1+ i) nt)
                      t)))))))
    (loop for x
          from (ceiling m 2)
            to m
          when (aref dp n x)
            do (println x)
            and do (return))))

#-swank (main)
