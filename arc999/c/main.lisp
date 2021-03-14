#+swank (declaim (optimize (speed 3) (safety 2)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro do-iota ((var count &optional (begin 0) (step 1)) &body body)
  (let ((cnt (gensym)))
    `(loop for ,cnt of-type fixnum below ,count
           with ,var of-type fixnum = ,begin
           do ,@body
              (incf (the fixnum ,var) (the fixnum ,step)))))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (princ obj stream)
    (terpri)))

(defun main ()
  (let ((n (read)))
    (println n)))

#+swank
(load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
