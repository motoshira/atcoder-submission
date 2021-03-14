(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt*
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (load "~/Dropbox/Code/atcoder/ac-tools/act.lisp")
  #+swank (ql:quickload :prove)
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
  (macrolet ((with-gensyms ((&rest args) &body body)
               `(let (,@(mapcar (lambda (arg) `(,arg (gensym))) args))
                  ,@body)))
    (with-gensyms (cnt q r)
      `(multiple-value-bind (,q ,r) (truncate ,count ,unroll)
         (declare (fixnum ,q ,r))
         (do ((,cnt 0 (the fixnum (1+ ,cnt))) (,var ,index-origin))
             ((>= ,cnt ,q) (loop repeat ,r do (progn ,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))
           (declare (fixnum ,cnt ,var))
           ,@(loop repeat unroll append `(,@body (setf (the fixnum ,var) (the fixnum (1+ ,var))))))))))

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

(defconstant +inf+ #.(expt 10 14))

;;;
;;; Body
;;;

(defun main ()
  (declare #.*opt*)
  (let* ((ax (read))
         (ay (read))
         (bx (read))
         (by (read))
         (fn (lambda (x y)
               (let ((x1 (- x ax))
                     (y1 (- y ay))
                     (x2 (- bx ax))
                     (y2 (- by ay)))
                 (- (* x1 y2)
                    (* x2 y1)))))
         (n (read))
         (points (loop repeat n collect (list (read) (read))
                         into res
                       finally (return `(,@res ,(first res))))))
    (declare (fixnum ax ay bx by n)
             (function fn)
             (list points))
    (dbg points)
    (println (nlet rec ((ps points)
                        (cnt 0))
               (cond
                 ((null (rest ps)) (1+ (ceiling cnt 2)))
                 (:else (dbg (apply fn (first ps))
                             (apply fn (second ps)))
                        (rec (rest ps)
                             (if (let ((s1 (apply fn (first ps)))
                                       (s2 (apply fn (second ps))))
                                   (assert (and (/= s1 0)
                                                (/= s2 0)))
                                   (or (and (> s1 0)
                                            (< s2 0))
                                       (and (< s1 0)
                                            (> s2 0))))
                                 (1+ cnt)
                                 cnt))))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
