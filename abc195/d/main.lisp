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
  (let* ((n (read))
         (m (read))
         (q (read))
         (items (loop repeat n collect (list (read) (read))))
         (xs (apply #'vector (loop repeat m collect (read))))
         (res (make-array (list (1+ m) (1+ m))))
         (qs (loop repeat q collect (list (1- (read)) (read)))))
    (setf items (sort items #'> :key #'second))
    (dotimes (l (1+ m))
      (dotimes (r (1+ m))
        (when (<= l r)
          (let ((ys (sort (concatenate 'list
                                       (loop for i below l
                                             collect (aref xs i))
                                       (loop for i from r below m
                                             collect (aref xs i)))
                          #'<)))
            (setf (aref res l r)
                  (nlet rec ((is items)
                             (ys ys)
                             (acc 0))
                    (cond
                      ((null is) acc)
                      (:else
                       (destructuring-bind (w v) (first is)
                         (loop for y in ys
                               when (<= w y)
                                 do (return-from rec
                                      (rec (rest is)
                                           (remove y ys :count 1)
                                           (+ acc v)))
                               finally (return-from rec
                                         (rec (rest is)
                                              ys
                                              acc))))))))))))
    (dbg res)
    (loop for (l r) in qs
          do (println (aref res l r)))))

#-swank
(progn
  (setf (sb-alien:extern-alien "thread_control_stack_size" sb-kernel::os-vm-size-t)
        (* 256 1024 1024))
  (sb-thread:join-thread (sb-thread:make-thread #'main)))
