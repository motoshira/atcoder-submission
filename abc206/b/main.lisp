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

#+swank
(defun take (fn amount &optional (index-origin 1))
  (loop for i below amount
        for idx = (+ i index-origin)
        collect (funcall fn idx)))

#+swank
(defun take-2d (fn height width)
  (let ((res (make-array (list height width))))
    (dotimes (i height)
      (dotimes (j width)
        (setf (aref res i j)
              (funcall fn i j))))
    res))

(declaim (inline calc))
(defun calc (n)
  (ash (* n (1+ n))
       -1))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri stream))))

(defun main ()
  (let ((n (read)))
    (loop for x from 1 to (expt 10 6)
          when (>= (calc x)
                   n)
            do (println x)
          and do (return-from main))))

#-swank (main)
