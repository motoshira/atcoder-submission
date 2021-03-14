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
  (let ((time (read)))
    (do-rep time
      (let ((n (read))
            (a (read))
            (b (read))
            (c (read))
            (d (read))
            (memo (make-hash-table :test #'eql)))
        (println (nlet rec ((x n))
                   (let ((tmp (gethash x memo)))
                     (when tmp
                       (return-from rec tmp)))
                   (setf (gethash x memo)
                         (cond
                           ((zerop x)
                            0)
                           ((minusp x)
                            +inf+)
                           (:else
                            (min (loop for (q cost) in `((2 ,a)
                                                         (3 ,b)
                                                         (5 ,c))
                                       for r = (rem x q)
                                       minimize (min (+ cost
                                                        (rec (floor (- x r)
                                                                    q))
                                                        (* r d))
                                                     (if (< (floor (+ x q (- r))
                                                                   q)
                                                            x)
                                                         (+ cost
                                                            (rec (floor (+ x q (- r))
                                                                        q))
                                                            (* (- q r)
                                                               d))
                                                         +inf+)))
                                 (* x d)))))))))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
