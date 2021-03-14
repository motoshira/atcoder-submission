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

(defmacro with-gensyms ((&rest args) &body body)
  `(let ,(mapcar (lambda (arg) `(,arg (gensym))) args)
     ,@body))

(defmacro do-bfs ((&rest args) &body body)
  (with-gensyms (front tail empty-p pop!)
    (let ((vars (mapcar #'first args))
          (inits (mapcar #'second args))
          (types (mapcar #'third args)))
      `(let ((,front nil)
             (,tail nil))
         (declare (list ,front ,tail))
         (labels ((call (&rest args)
                    (push args ,tail))
                  (,empty-p ()
                    (and (null ,front)
                         (null ,tail)))
                  (,pop! ()
                    (when (null ,front)
                      (setf ,front (nreverse ,tail)
                            ,tail nil))
                    (pop ,front)))
           (declare (inline call ,empty-p ,pop!))
           (call ,@inits)
           (loop until (,empty-p)
                 for ,vars
                   of-type ,(or types (loop repeat (length args) collect t)) = (,pop!)
                 do ,@body))))))


;;;
;;; Body
;;;

(defun main ()
  (declare #.*opt*)
  (let* ((n (read))
         (edges (make-array (1+ n) :element-type 'list :initial-element nil)))
    ;; 1-indexed
    (do-rep (1- n)
      (let ((a (read))
            (b (read)))
        (push b (aref edges a))
        (push a (aref edges b))))
    (let ((ss (make-array (1+ n) :initial-element +inf+))
          (res 0)
          (cs (loop repeat n collect (read))))
      (setf cs (sort cs #'>))
      (do-bfs ((node 1)
               (parent -1))
        (setf #1=(aref ss node) (pop cs))
        (when (/= -1 parent)
          (incf res (min #1# (aref ss parent))))
        (dolist (nxt (aref edges node))
          (when (/= nxt parent)
            (call nxt node))))
      (println res)
      (dotimes! (i n 1)
        (when (> i 1)
          (princ #\Space))
        (princ (aref ss i)))
      (fresh-line))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
