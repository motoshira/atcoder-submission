(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #-swank (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #-swank (sb-ext:disable-debugger)
  (pushnew :inline-generic-funcion *features*))

(defmacro dbg (&rest forms)
  #-swank (declare (ignore forms))
  #+swank `(format *error-output* "~a => ~a~&" ',forms `(,,@forms)))

(defmacro do-rep (count &body body) `(loop repeat ,count do ,@body))

(defmacro nlet (name binds &body body)
  `(labels ((,name (,@(mapcar #'first binds))
              ,@body))
     (,name ,@(mapcar #'second binds))))

(defmacro dotimes! ((var count &optional (index-origin 0) (unroll 60)) &body body)
  (let ((cnt (gensym))
        (q (gensym))
        (r (gensym)))
    `(multiple-value-bind (,q ,r) (truncate ,count ,unroll)
       (declare (fixnum ,q ,r))
       (do ((,cnt 0 (the fixnum (1+ ,cnt))) (,var ,index-origin))
           ((>= ,cnt ,q) (loop repeat ,r do (progn ,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))
         (declare (fixnum ,cnt ,var))
         ,@(loop repeat unroll append `(,@body (setf (the fixnum ,var) (the fixnum (1+ ,var)))))))))
 
(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 obj
      (princ obj stream)
      (terpri))))

(defun read-nums (count &optional (element-type '(simple-array fixnum (*))))
  (declare (fixnum count))
  (coerce (loop repeat count collect (read)) element-type))

(define-modify-macro maxf (var) max)
(define-modify-macro minf (var) min)

(defvar *magic* #.(expt 10 6))

(defun main ()
  (declare #.*opt*)
  (do-rep (read)
    (let ((l (read))
          (r (read)))
      (declare (fixnum l r))
      (let ((x (+ r (- (* l 2)) 1)))
        (println (if (minusp x)
                     0
                     (ash (* x (1+ x)) -1)))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
